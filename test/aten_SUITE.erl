-module(aten_SUITE).

-export([
    all/0,
    groups/0,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    distribution_flood/1,
    detect_node_partition/1,
    detect_node_stop_start/1,
    unregister_does_not_detect/1,
    register_unknown_emits_down/1,
    register_detects_down/1,
    watchers_cleanup/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(POLLINT, 1000).
-define(HBINT, 250).
-define(SCALE, 1.5).

all() ->
    [
     {group, tests}
    ].

all_tests() ->
    [
     distribution_flood,
     detect_node_partition,
     detect_node_stop_start,
     unregister_does_not_detect,
     register_unknown_emits_down,
     register_detects_down,
     watchers_cleanup
    ].

groups() ->
    [
     {tests, [], all_tests()}
    ].

init_per_group(_, Config) ->
    _ = application:load(aten),
    ok = application:set_env(aten, poll_interval, ?POLLINT),
    ok = application:set_env(aten, heartbeat_interval, ?HBINT),
    ok = application:set_env(aten, scaling_factor, ?SCALE),
    application:ensure_all_started(aten),
    Config.

end_per_group(_, Config) ->
    _ = application:stop(ra),
    Config.

init_per_testcase(_TestCase, Config) ->
    meck:new(aten_sink, [passthrough]),
    application:stop(aten),
    application:start(aten),
    Config.

end_per_testcase(_Case, _Config) ->
    meck:unload(),
    ok.

echo_proc() ->
    receive
        {P, Data} ->
            P ! Data,
            echo_proc();
        stop ->
            ok
    end.


sink_proc() ->
    receive
        _ ->
            sink_proc()
    end.

load_proc(EPid, SPid, Data) ->
    EPid ! {SPid, Data},
    load_proc(EPid, SPid, Data).

distribution_flood(_Config) ->
    S1 = make_node_name(?FUNCTION_NAME),
    ok = aten:register(S1),
    receive
        {node_event, S1, down} -> ok
    after 5000 ->
              exit(node_event_timeout)
    end,
    {ok, P1, S1} = start_peer(?FUNCTION_NAME),
    ct:pal("Node ~w Nodes ~w", [node(), nodes()]),
    receive
        {node_event, S1, up} -> ok
    after 5000 ->
              exit(node_event_timeout)
    end,
    timer:sleep(30000),
    ct:pal("PRE nosuspends ~b",
           [gen_server:call({aten_emitter, S1}, dummy)]),

    %% generate some load on the distribution channel
    ct:pal("flooding..."),
    EPid = spawn(S1, fun echo_proc/0),
    SPid = spawn(fun sink_proc/0),
    Data = crypto:strong_rand_bytes(8024 * 2),
    LPid = spawn(fun () -> load_proc(EPid, SPid, Data) end),
    receive
        {node_event, S1, down} ->
            ct:pal("DOWN!! nosuspends ~b",
                   [gen_server:call({aten_emitter, S1}, dummy)]),
            % %% check if it changes
            receive
                {node_event, S1, up} ->
                    ct:pal("UP again!"),
                    ok
            after ?POLLINT + 20 ->
                      flush(),
                      peer:stop(P1),
                      exit(unexpected_down)
            end
    after 60000 ->
              ct:pal("NO DOWN nosuspends ~b", [gen_server:call({aten_emitter, S1}, dummy)]),
              ok
    end,

    exit(LPid, normal),
    exit(SPid, normal),
    exit(EPid, normal),
    peer:stop(P1),
    ok.


detect_node_partition(_Config) ->
    S1 = make_node_name(?FUNCTION_NAME),
    ok = aten:register(S1),
    receive
        {node_event, S1, down} -> ok
    after 5000 ->
              exit(node_event_timeout)
    end,
    {ok, P1, S1} = start_peer(?FUNCTION_NAME),
    ct:pal("Node ~w Nodes ~w", [node(), nodes()]),
    receive
        {node_event, S1, up} -> ok
    after 5000 ->
              exit(node_event_timeout)
    end,
    %% give it enough time to generate more than one sample
    timer:sleep(2000),
    simulate_partition(S1),

    receive
        {node_event, S1, down} -> ok
    after 5000 ->
              flush(),
              exit(node_event_timeout)
    end,
    meck:unload(aten_sink),

    receive
        {node_event, S1, up} -> ok
    after 5000 ->
              flush(),
              exit(node_event_timeout)
    end,
    ok = peer:stop(P1),
    ok = aten:unregister(S1),
    ok.

detect_node_stop_start(_Config) ->
    S1 = make_node_name(?FUNCTION_NAME),
    ok = aten:register(S1),
    {ok, P1, S1} = start_peer(?FUNCTION_NAME),
    ct:pal("Node ~w Nodes ~w", [node(), nodes()]),
    receive
        {node_event, S1, up} -> ok
    after 5000 ->
        exit(node_event_timeout)
    end,

    %% give it enough time to generate more than one sample
    timer:sleep(1000),

    ok = peer:stop(P1),
    receive
        {node_event, S1, down} -> ok
    after 5000 ->
        exit(node_event_timeout)
    end,

    {ok, P2, S1} = start_peer(?FUNCTION_NAME),
    receive
        {node_event, S1, up} -> ok
    after 5000 ->
        exit(node_event_timeout)
    end,
    ok = peer:stop(P2),
    ok = aten:unregister(S1),
    ok.

unregister_does_not_detect(_Config) ->
    S1 = make_node_name(?FUNCTION_NAME),
    S2 = make_node_name(unregister_does_not_detect_2),
    ok = aten:register(S1),
    ok = aten:register(S2),
    wait_for({node_event, S1, down}),
    wait_for({node_event, S2, down}),
    {ok, P1, S1} = start_peer(?FUNCTION_NAME),
    {ok, P2, S2} = start_peer(unregister_does_not_detect_2),
    ct:pal("Node ~w Nodes ~w", [node(), nodes()]),
    wait_for({node_event, S1, up}),
    wait_for({node_event, S2, up}),
    {monitored_by, MonByPids} = erlang:process_info(self(), monitored_by),
    DetectorPid = whereis(aten_detector),
    %% one monitor for the process by each node
    ?assertEqual(2, length([P || P <- MonByPids, P == DetectorPid])),
    ok = aten:unregister(S1),
    %% aten:unmregister is a cast so we need to call in before asserting
    gen_server:call(aten_detector, any),
    {monitored_by, MonByPidsAfter} = erlang:process_info(self(), monitored_by),
    ?assertEqual(1, length([P || P <- MonByPidsAfter, P == DetectorPid])),
    peer:stop(P1),
    receive
        {node_event, S1, Evt} ->
            exit({unexpected_node_event, S1, Evt})
    after 1000 ->
        ok
    end,
    peer:stop(P2),
    wait_for({node_event, S2, down}),
    ok.

register_unknown_emits_down(_Config) ->
    S1 = make_node_name(?FUNCTION_NAME),
    ok = aten:register(S1),
    receive
        {node_event, S1, down} -> ok
    after 5000 ->
        exit(node_event_timeout)
    end,
    ok = aten:unregister(S1),
    ok.

register_detects_down(_Config) ->
    S1 = make_node_name(?FUNCTION_NAME),
    ok = aten:register(S1),
    receive
        {node_event, S1, down} -> ok
    after 5000 ->
              exit(node_event_timeout)
    end,
    {ok, P1, S1} = start_peer(?FUNCTION_NAME),
    receive
        {node_event, S1, up} -> ok
    after 5000 ->
              flush(),
              exit(node_event_timeout_2)
    end,
    simulate_partition(S1),
    receive
        {node_event, S1, down} -> ok
    after 5000 ->
              flush(),
              exit(node_event_timeout_3)
    end,
    ok = aten:unregister(S1),
    %% re-register should detect down
    ok = aten:register(S1),
    receive
        {node_event, S1, down} -> ok
    after 5000 ->
              exit(node_event_timeout_4)
    end,
    ok = aten:unregister(S1),

    peer:stop(P1),
    ok.

watchers_cleanup(_Config) ->
    Node = make_node_name(?FUNCTION_NAME),
    Self = self(),
    Watcher = spawn_watcher(Node, Self),
    ok = aten:register(Node),
    %% first clear out all the initial notifications
    receive
        {watcher_node_down, Node} -> ok
    after 5000 ->
        exit(node_event_timeout)
    end,
    receive
        {node_event, Node, down} -> ok
    after 5000 ->
        exit(node_event_timeout)
    end,
    {ok, Peer, Node} = start_peer(?FUNCTION_NAME),
    ct:pal("Node ~w Nodes ~w", [node(), nodes()]),
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
    ok = peer:stop(Peer),

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

    State2 = sys:get_state(aten_sink),
    NodeMap = element(2, State2),
    none = maps:get(Node, NodeMap, none),

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
                        ct:pal("Dropping hb from ~w", [Node]),
                        {noreply, State};
                    (Msg, State) ->
                        aten_sink:handle_cast(Msg, State)
                end).

get_current_host() ->
    N = atom_to_list(node()),
    {ok, list_to_atom(after_char($@, N))}.

make_node_name(N) ->
    {ok, Host} = get_current_host(),
    list_to_atom(lists:flatten(io_lib:format("~s@~s", [N, Host]))).

start_peer(N) ->
    Pa = filename:dirname(code:which(aten)),
    ct:pal("starting node ~w with ~s", [N, Pa]),
    {ok, P, S} = ?CT_PEER(#{name => N, args => ["-pa", Pa]}),
    ok = rpc:call(S, application, load, [aten]),
    ok = rpc:call(S, application, set_env, [aten, poll_interval, ?POLLINT]),
    ok = rpc:call(S, application, set_env, [aten, heartbeat_interval, ?HBINT]),
    ok = rpc:call(S, application, set_env, [aten, scaling_factor, ?SCALE]),
    {ok, _} = rpc:call(S, application, ensure_all_started, [aten]),
    {ok, P, S}.

after_char(_, []) -> [];
after_char(Char, [Char|Rest]) -> Rest;
after_char(Char, [_|Rest]) -> after_char(Char, Rest).


flush() ->
    receive M ->
                ct:pal("flushed ~w", [M]),
                flush()
    after ?POLLINT ->
              ok
    end.

wait_for(Evt) ->
    receive
        Evt -> ok
    after 5000 ->
              exit({wait_for_timeout, Evt})
    end.
