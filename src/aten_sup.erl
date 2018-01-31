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
