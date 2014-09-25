-module(siftbulk_app).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, _) ->
    siftbulk_sup:start_link().

stop(_State) ->
    ok.
