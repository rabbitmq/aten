-module(aten_sink).

-behaviour(gen_server).

%% API functions
-export([start_link/0,
         get_failure_probabilities/0,
         beat/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {data = #{} :: #{node() => aten_detect:state()}}).
-type state() :: #state{}.

% -define(INTERVAL, 100).


%%% aten_emitter - emits heartbeats to all connected nodes periodically

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_failure_probabilities() ->
    gen_server:call(?MODULE, get_failure_probabilities).

-spec beat(node()) -> ok.
beat(DestNode) ->
    Dest = {?MODULE, DestNode},
    Msg = {hb, node()},
    _ = erlang:send(Dest, {'$gen_cast', Msg}, [noconnect, nosuspend]),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(term()) -> {ok, state()}.
init([]) ->
    {ok, #state{}}.

handle_call(get_failure_probabilities, From, State) ->
    Data = State#state.data,
    % reply in a different process as we don't want calculation times
    % to affect the sample time of any incoming heartbeats
    _ = spawn(fun () ->
                      Probs = get_probabilities(Data),
                      gen_server:reply(From, Probs)
              end),
    {noreply, State}.

handle_cast({hb, Node}, #state{data = Data0} = State) ->
    Data = maps:update_with(Node,
                            fun (S) -> aten_detect:sample_now(S) end,
                            aten_detect:init(), Data0),
    {noreply, State#state{data = Data}}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_probabilities(Data) ->
    maps:map(fun (_Key, Value) ->
                     aten_detect:get_failure_probability(Value)
             end, Data).
