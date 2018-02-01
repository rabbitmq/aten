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
     detect_node_up_down_up,
     unregister_does_not_detect
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
    application:stop(aten),
    application:start(aten),
    Config.


detect_node_up_down_up(_Config) ->
    S1 = make_node_name(s1),
    {ok, S1} = start_slave(s1),
    ok = aten:register(S1),
    ct:pal("Nodes ~p", [nodes()]),
    receive
        {node_event, S1, up} -> ok
    after 2000 ->
              exit(node_event_timeout)
    end,
    ok = slave:stop(S1),
    receive
        {node_event, S1, down} -> ok
    after 2000 ->
              exit(node_event_timeout)
    end,
    {ok, S1} = start_slave(s1),
    receive
        {node_event, S1, up} -> ok
    after 2000 ->
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
    after 2000 ->
              exit(node_event_timeout)
    end,
    ok = aten:unregister(S1),
    receive
        {node_event, S1, Evt} ->
            exit({unexpected_node_event, S1, Evt})
    after 2000 ->
              ok
    end,
    ok.

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
