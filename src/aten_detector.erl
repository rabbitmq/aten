-module(aten_detector).

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

-record(state, {tref :: reference() | undefined}).
% -type state() :: #state{}.

-define(POLL_INTERVAL_MS, 1000).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, set_timer(#state{})}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(poll, State) ->
    Probs = aten_sink:get_failure_probabilities(),
    error_logger:info_msg("atens_detector info ~p~n", [Probs]),
    {noreply, set_timer(State)}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

set_timer(State) ->
    TRef = erlang:send_after(?POLL_INTERVAL_MS, self(), poll),
    State#state{tref = TRef}.
