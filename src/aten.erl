%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2018-2023 Broadcom. All Rights Reserved. The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
%%
-module(aten).

-export([
         start/0,
         register/1,
         unregister/1
        ]).

start() ->
    application:ensure_all_started(aten).

-spec register(node()) -> ok | ignore.
register(Node) when Node == node() ->
    ignore;
register(Node) ->
    aten_detector:register(Node).

-spec unregister(node()) -> ok | ignore.
unregister(Node) when Node == node() ->
    ignore;
unregister(Node) ->
    aten_detector:unregister(Node).
