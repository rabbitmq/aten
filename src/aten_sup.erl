%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2018-2020 VMware, Inc. or its affiliates.  All rights reserved.
%%
-module(aten_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    AtenSink = #{id => aten_sink,
                 start => {aten_sink, start_link, []}},
    AtenEmitter = #{id => aten_emitter,
                    start => {aten_emitter, start_link, []}},
    AtenDetector = #{id => aten_detector,
                    start => {aten_detector, start_link, []}},
	Procs = [AtenSink, AtenEmitter, AtenDetector],
	{ok, {{one_for_one, 1, 5}, Procs}}.
