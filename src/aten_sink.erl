-module(aten_sink).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

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

-record(state, {data = #{} :: #{node() => aten_detect:state()},
                monitors = #{} :: #{node() => boolean()}}).
-type state() :: #state{}.

%%% aten_sink

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
                      try
                          Probs = get_probabilities(Data),
                          gen_server:reply(From, Probs)
                      catch ErrType:Error ->
                          ?LOG_ERROR("get_probabilities error: ~p:~P", [ErrType, Error, 10])
                      end
              end),
    {noreply, State}.

handle_cast({hb, Node}, #state{data = Data0, monitors = Monitors0} = State) ->
    Monitors = maybe_monitor_node(Node, Monitors0),
    Data = case Data0 of
               #{Node := S} ->
                   Data0#{Node => aten_detect:sample_now(S)};
               _ ->
                   Data0#{Node => aten_detect:init()}
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
