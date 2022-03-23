%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2018-2020 VMware, Inc. or its affiliates.  All rights reserved.
%%
-module(aten_sink).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API functions
-export([start_link/0,
         get_failure_probabilities/0,
         beat/1,
         beat_blocking/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {data = #{} :: #{node() => aten_detect:state()},
                monitors = #{} :: #{node() => boolean()},
                factor = 1.5 :: float()}).
-type state() :: #state{}.

%%% aten_sink

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_failure_probabilities() ->
    Data = gen_server:call(?MODULE, get_data),
    get_probabilities(Data).

-spec beat(node()) -> ok | noconnect | nosuspend.
beat(DestNode) ->
    Dest = {?MODULE, DestNode},
    Msg = {hb, node()},
    erlang:send(Dest, {'$gen_cast', Msg}, [noconnect, nosuspend]).

-spec beat_blocking(node()) -> ok.
beat_blocking(DestNode) ->
    Dest = {?MODULE, DestNode},
    Msg = {hb, node()},
    _ = gen_server:cast(Dest, Msg),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(term()) -> {ok, state()}.
init([]) ->
    F = application:get_env(aten, scaling_factor, 1.5),
    {ok, #state{factor = F}}.

handle_call(get_data, _From, State) ->
    {reply, State#state.data, State}.

handle_cast({hb, Node}, #state{data = Data0,
                               monitors = Monitors0,
                               factor = Factor} = State) ->
    Monitors = maybe_monitor_node(Node, Monitors0),
    Data = case Data0 of
               #{Node := S} ->
                   Data0#{Node => aten_detect:sample_now(S)};
               _ ->
                   Data0#{Node => aten_detect:init(Factor)}
           end,
    {noreply, State#state{data = Data, monitors = Monitors}}.

handle_info({nodedown, Node}, #state{data = Data0, monitors = Monitors0} = State) ->
    % Note: do NOT unregister the node monitor here - it is unnecessary
    % and will actually end up trying to re-connect to the node
    % resulting in an infinite number of nodedown messages
    Data = maps:remove(Node, Data0),
    Monitors = maps:remove(Node, Monitors0),
    {noreply, State#state{data = Data, monitors = Monitors}};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

maybe_monitor_node(Node, Monitors) ->
    IsMonitored = maps:get(Node, Monitors, false),
    case IsMonitored of
        true -> Monitors;
        false ->
            Monitors#{Node => erlang:monitor_node(Node, true)}
    end.

get_probabilities(Data) ->
    maps:map(fun (_Key, Value) ->
                     aten_detect:get_failure_probability(Value)
             end, Data).
