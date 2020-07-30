%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2018-2020 VMware, Inc. or its affiliates.  All rights reserved.
%%
-module(aten_detect).

-export([
         init/1,
         sample_now/1,
         get_failure_probability/1
         ]).

-type sample() :: integer().

-define(WINDOW_SIZE, 1000).

-record(state, {freshness :: undefined | non_neg_integer(),
                samples = array:new(?WINDOW_SIZE) :: array:array(sample()),
                next_index = 0 :: non_neg_integer(),
                max_size = 1000 :: non_neg_integer(),
                factor = 1.5 :: number()}).


-opaque state() :: #state{}.

-export_type([state/0]).

init(Factor) ->
    #state{factor = Factor}.

-spec sample_now(state()) -> state().
sample_now(State) ->
    append(ts(), State).

-spec get_failure_probability(state()) -> float().
get_failure_probability(State) ->
    failure_prob_at(ts(), State).

%% Internal

append(Ts, #state{freshness = undefined} = State) ->
    State#state{freshness = Ts};
append(Ts0, #state{freshness = F,
                   samples = Samples,
                   next_index = Next} = State) when is_number(F) ->
    Ts = Ts0 - F,
    State#state{samples = array:set(Next, Ts, Samples),
                next_index = (Next + 1) rem ?WINDOW_SIZE,
                freshness = Ts0}.

failure_prob_at(_At, #state{freshness = undefined}) ->
    0.0;
failure_prob_at(At, #state{freshness = F,
                           factor = A,
                           samples = Samples}) ->
    T = At - F,
    {TotNum, SmallNum} = array:foldl(
                           fun(_, undefined, Acc) ->
                                   Acc;
                              (_, S, {Tot, Smaller}) when S * A =< T ->
                                   {Tot+1, Smaller+1};
                              (_, _S, {Tot, Smaller}) ->
                                   {Tot+1, Smaller}
                           end, {0, 0}, Samples),
    SmallNum / max(1, TotNum). % avoid div/0

ts() ->
    % TODO: should we use erlang monotonic time instead?
    % It probably doesn't matter
    erlang:system_time(microsecond).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

detect_test() ->
    S0 = #state{},
    ?assert(failure_prob_at(10, S0) =:= 0.0),
    S = lists:foldl(fun append/2, S0, [1, 5, 4, 10, 13, 20, 25]),
    ?assert(failure_prob_at(28, S) < 0.5),
    ?assert(failure_prob_at(40, S) == 1.0),
    S1 = append(10, S0),
    ?assertEqual(0.0, failure_prob_at(10, S1)),
    % we cannot detect failures with only a single sample
    % ?assert(failure_prob_at(100, S1) > 0.0),
    ok.

-endif.
