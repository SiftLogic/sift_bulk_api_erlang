%% Supervises an instance of a worker that connects to the ftp server.
-module(siftbulk_client_sup).
-behaviour(supervisor).
-export([start_link/0, init/1, add_child/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    Restart = {one_for_one, 2, 3},
    {ok, {Restart, []}}.

add_child() -> 
    NewChild = {siftbulk_worker,
                {siftbulk_client, start_link, []},
                transient,
                200, % ms
                worker,
                [siftbulk_client]
               },
    supervisor:start_child(self(), NewChild).
