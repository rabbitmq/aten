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
                interval = ?INTERVAL :: non_neg_integer() }).
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
    Interval =  application:get_env(aten, heartbeat_interval, ?INTERVAL),
    {ok, set_timer(#state{interval = Interval})}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(emit_heartbeats, State) ->
    _ = [ok = aten_sink:beat(Node) || Node <- nodes()],
    {noreply, set_timer(State)}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
set_timer(State) ->
    TRef = erlang:send_after(State#state.interval, self(), emit_heartbeats),
    State#state{tref = TRef}.
