%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2018-2020 VMware, Inc. or its affiliates.  All rights reserved.
%%
-module(aten_emitter).

-behaviour(gen_server).

%% API functions
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(INTERVAL, 100).

-record(state, {tref :: reference() | undefined,
                counters :: counters:counters_ref(),
                interval = ?INTERVAL :: non_neg_integer(),
                %% nodes for which a hearbeat is currently being sent in a separate
                %% process without nosuspend
                blocked = [] :: [node()]}).

-type state() :: #state{}.



%%% aten_emitter - emits heartbeats to all connected nodes periodically

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(term()) -> {ok, state()}.
init([]) ->
    Interval = application:get_env(aten, heartbeat_interval, ?INTERVAL),
    {ok, update_state(#state{interval = Interval,
                             counters = counters:new(1, [])},
                      [])}.

handle_call(_Request, _From, State) ->
    Reply = counters:get(State#state.counters, 1),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(emit_heartbeats,
            #state{counters = Cnt,
                   blocked = Blocked0} = State) ->
    Blocked = lists:foldl(
                fun (Node, Acc) ->
                        case aten_sink:beat(Node) of
                            nosuspend ->
                                counters:add(Cnt, 1, 1),
                                Self = self(),
                                spawn(fun () ->
                                              ok = aten_sink:beat_blocking(Node),
                                              Self ! {unblock, Node},
                                              ok
                                      end),
                                [Node | Acc];
                            _ ->
                                Acc
                        end
                end, Blocked0, nodes() -- Blocked0),
    {noreply, update_state(State, Blocked)};
handle_info({unblock, Node}, #state{blocked = Blocked} = State) ->
    {noreply, State#state{blocked = lists:delete(Node, Blocked)}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
update_state(State, Blocked) ->
    TRef = erlang:send_after(State#state.interval, self(), emit_heartbeats),
    State#state{tref = TRef,
                blocked = Blocked}.
