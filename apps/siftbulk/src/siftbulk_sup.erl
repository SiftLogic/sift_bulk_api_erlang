%% Supervises all the clients
-module(siftbulk_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Restart = {one_for_all, 2, 5},
    Child0 = {siftbulk,
          {siftbulk, start_link, []},
          permanent,
          200, % ms
          worker,
          [siftbulk]
         },
     Child1 = {siftbulk_client_sup,
      {siftbulk_client_sup, start_link, []},
      permanent,
      200, % ms
      supervisor,
      [siftbulk_client_sup]
     },
    {ok,{Restart, [Child0, Child1]}}.
