%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2018-2020 VMware, Inc. or its affiliates.  All rights reserved.
%%
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
