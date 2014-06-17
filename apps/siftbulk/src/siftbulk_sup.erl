-module(siftbulk_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

init(_Args) ->
    Restart = {one_for_one, 2, 5},
    C0 = {siftbulk,
          {siftbulk, start_link, []},
          permanent,
          200, % ms
          worker,
          [siftbulk]
         },
    {ok,{Restart, [C0]}}.


