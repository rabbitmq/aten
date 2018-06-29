-module(aten_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
     {group, tests}
    ].

all_tests() ->
    [
     detect_node_partition,
     detect_node_stop_start,
     unregister_does_not_detect,
     watchers_cleanup
    ].

groups() ->
    [
     {tests, [], all_tests()}
    ].

init_per_group(_, Config) ->
    _ = application:load(aten),
    ok = application:set_env(aten, poll_interval, 500),
    application:ensure_all_started(aten),
    Config.

end_per_group(_, Config) ->
    _ = application:stop(ra),
    Config.

init_per_testcase(_TestCase, Config) ->
    % try to stop all slaves
    [begin
         slave:stop(N),
         ok = aten:unregister(N)
     end || N <- nodes()],
    meck:new(aten_sink, [passthrough]),
    application:stop(aten),
    application:start(aten),
    Config.

end_per_testcase(_Case, _Config) ->
    meck:unload(),
    ok.


detect_node_partition(_Config) ->
    S1 = make_node_name(s1),
    {ok, S1} = start_slave(s1),
    ok = aten:register(S1),
    ct:pal("Nodes ~w", [nodes()]),
    receive
        {node_event, S1, up} -> ok
    after 5000 ->
              exit(node_event_timeout)
    end,
    %% give it enough time to generate more than one sample
    timer:sleep(1000),
    simulate_partition(S1),

    receive
        {node_event, S1, down} -> ok
    after 5000 ->
              exit(node_event_timeout)
    end,
    meck:unload(aten_sink),

    receive
        {node_event, S1, up} -> ok
    after 5000 ->
              exit(node_event_timeout)
    end,
    ok =  slave:stop(S1),
    ok = aten:unregister(S1),
    ok.

detect_node_stop_start(_Config) ->
    S1 = make_node_name(s1),
    ok = aten:register(S1),
    {ok, S1} = start_slave(s1),
    ct:pal("Nodes ~w", [nodes()]),
    receive
        {node_event, S1, up} -> ok
    after 5000 ->
              exit(node_event_timeout)
    end,
    %% give it enough time to generate more than one sample
    timer:sleep(1000),

    ok = slave:stop(S1),
    receive
        {node_event, S1, down} -> ok
    after 5000 ->
              exit(node_event_timeout)
    end,

    {ok, S1} = start_slave(s1),
    receive
        {node_event, S1, up} -> ok
    after 5000 ->
              exit(node_event_timeout)
    end,
    ok =  slave:stop(S1),
    ok = aten:unregister(S1),
    ok.

unregister_does_not_detect(_Config) ->
    S1 = make_node_name(s1),
    ok = aten:register(S1),
    {ok, S1} = start_slave(s1),
    ct:pal("Nodes ~p", [nodes()]),
    receive
        {node_event, S1, up} -> ok
    after 5000 ->
              exit(node_event_timeout)
    end,
    ok = aten:unregister(S1),
    receive
        {node_event, S1, Evt} ->
            exit({unexpected_node_event, S1, Evt})
    after 5000 ->
              ok
    end,
    ok.

watchers_cleanup(_Config) ->
    Node = make_node_name(s1),
    Self = self(),
    Watcher = spawn_watcher(Node, Self),
    ok = aten:register(Node),
    {ok, Node} = start_slave(s1),
    receive
        {watcher_node_up, Node} -> ok
    after 5000 ->
        exit(node_event_timeout)
    end,
    receive
        {node_event, Node, up} -> ok
    after 5000 ->
        exit(node_event_timeout)
    end,

    State0 = sys:get_state(aten_detector),
    Watchers0 = element(6, State0),
    #{Node := #{Watcher := _}} = Watchers0,
    #{Node := #{Self := _}} = Watchers0,

    Watcher ! stop,

    timer:sleep(200),
    ok = slave:stop(Node),

    receive
        {watcher_node_down, Node} ->
            exit(stopped_watcher_receive_message)
    after 50 ->
            ok
    end,
    receive
        {node_event, Node, down} -> ok
    after 5000 ->
        exit(node_event_timeout)
    end,

    State1 = sys:get_state(aten_detector),
    Watchers1 = element(6, State1),
    #{Node := Pids} = Watchers1,
    #{Node := #{Self := _}} = Watchers1,
    none = maps:get(Watcher, Pids, none),
    ok = aten:unregister(Node).

spawn_watcher(Node, Pid) ->
    spawn(fun Fun() ->
        ok = aten:register(Node),
        receive
            {node_event, Node, up} ->
                Pid ! {watcher_node_up, Node},
                Fun();
            {node_event, Node, down} ->
                Pid ! {watcher_node_down, Node},
                Fun();
            stop -> ok
        end
    end).


%% simulates a partition from a remote node by dropping messages
%% received from some specific node
simulate_partition(Node) ->
    meck:expect(aten_sink, handle_cast,
                fun ({hb, N}, State) when N =:= Node ->
                        %% drop this message
                        ct:pal("Dropping hb from ~w~n", [Node]),
                        {noreply, State};
                    (Msg, State) ->
                        aten_sink:handle_cast(Msg, State)
                end).

get_current_host() ->
    {ok, H} = inet:gethostname(),
    list_to_atom(H).

make_node_name(N) ->
    {ok, H} = inet:gethostname(),
    list_to_atom(lists:flatten(io_lib:format("~s@~s", [N, H]))).

search_paths() ->
    Ld = code:lib_dir(),
    lists:filter(fun (P) -> string:prefix(P, Ld) =:= nomatch end,
                 code:get_path()).
start_slave(N) ->
    Host = get_current_host(),
    Pa = string:join(["-pa" | search_paths()] ++ ["-s aten"], " "),
    ct:pal("starting slave node with ~s~n", [Pa]),
    slave:start_link(Host, N, Pa).
