%% @doc Contains all the operations to upload, poll and download files.

-module(siftbulk).
-behaviour(gen_server).

-record(state, {parent_pid = undefined,
                 worker_sup = undefined}).

-export([start_link/0, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([init/1, connect/0, stop/0]).

%% Private

%%

%% Starts the API server.

-spec start_link() -> {atom(), pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Initializes the manager of siftbulk workers.

-spec init([]) -> {ok, #state{}}.
init([]) ->
    {ok, #state{}}.

%% Creates a worker who connects to the server.

-spec connect() -> {atom(), pid()} | {error, atom()}.
connect() ->
    gen_server:call(?MODULE, connect).

%% Handles all synchronous calls to the server. See specific functions for more
%% detailed information.

handle_call(connect, _From, #state{parent_pid = ParentPid} = State) ->
    NewChild = {{siftbulk_client, now()},
                {siftbulk_client, start_link, []},
                transient,
                200, % ms
                worker,
                [siftbulk_client]
               },
    {ok, ClientPid} = supervisor:start_child(siftbulk_client_sup, NewChild),
    Response = case gen_server:call(ClientPid, connect) of
                   ok ->
                       {ok, ClientPid};
                   {error, Error} ->
                       supervisor:terminate_child(siftbulk_client_sup, ClientPid),
                      {error, Error}
               end,
    {reply, Response, State#state{parent_pid = ParentPid}};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.
 
%% Handles all asynchronous calls to the server. See specific functions for more
%% detailed information.

-spec handle_cast(any(), any()) -> {noreply, any()}.
handle_cast(_Msg, State) ->
    {noreply, State}.
 
%% Handles direct messages to this server.

-spec handle_info(any(), any()) -> {noreply, any()}.
handle_info(_Msg, State) ->
    {noreply, State}.

%% Handles hot code upgrades. Currently, does nothing.

-spec code_change(any(), any(), any()) -> {ok, any()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% Handles stopping of the server. Currently, just returns nothing.

-spec terminate(any(), any()) -> ok.
terminate(_Reason, _State) ->
    ok.

% Stops this server

-spec stop() -> ok.
stop() ->
    gen_server:call(?MODULE, stop).
