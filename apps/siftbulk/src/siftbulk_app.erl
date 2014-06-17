-module(siftbulk_app).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, Args) ->
    siftbulk_sup:start_link(Args).

stop(_State) ->
    ok.
