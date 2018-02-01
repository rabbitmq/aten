-module(aten).

-export([
         start/0,
         register/1,
         unregister/1
         ]).

-export_type([
              ]).


start() ->
    application:ensure_all_started(aten).

-spec register(node()) -> ok.
register(Node) ->
    aten_detector:register(Node).

-spec unregister(node()) -> ok.
unregister(Node) ->
    aten_detector:unregister(Node).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
