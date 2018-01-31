-module(aten).

-export([
         start/0
         ]).

-export_type([
              ]).


start() ->
    application:ensure_all_started(aten).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
