%% @doc Contains all the operations to upload, poll and download files.

-module(siftbulk).
-behaviour(gen_server).
-include("../include/siftbulk.hrl").

-export([start_link/0, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([set_opts/1, get_opts/0, init/1, stop/0]).

-type connection_part() :: {atom(), any()}.
-type connection_list() :: list(connection_part()).
-type state() :: #state{}.

%% Private

-spec set_opts(state(), connection_list()) -> state().
set_opts(#state{} = State, []) ->
    State;
set_opts(#state{} = State, Opts) ->
    Fields = [host, password, poll_every, port, username],
    set_opts(State, Fields, Opts).

-spec set_opts(state(), list(), connection_list()) -> state().
set_opts(#state{} = State, [], _Opts) ->
    State;
set_opts(#state{opts = OldOpts} = State, [Field | Tail], Opts) ->
    case lists:keyfind(Field, 1, Opts) of
        {Field, _} = NewValue ->
            case lists:keyfind(Field, 1, OldOpts) of
                false -> 
                    set_opts(
                        State#state{opts = [NewValue | OldOpts]},
                        Tail, Opts);
                _ ->
                    set_opts(
                        State#state{opts = lists:keyreplace(Field, 1, OldOpts, NewValue)},
                        Tail, Opts)
            end;
        false ->
            set_opts(State, Tail, Opts)
    end.

%% Starts the API server.

-spec start_link() -> {atom(), pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Initializes the API server with FTP connection options:
%% user: The username to get into the ftp server.
%% password: The password to get into the ftp server.
%% host: The host to connect to. Defaults to localhost.
%% port: The port to connect to. Defaults to 21.
%% poll_every: Number of seconds to poll. Defaults to 300 (5 minutes)if falsey.

-spec init([]) -> {ok, state()}.
init([]) ->
    Opts = case application:get_env(siftbulk, auth) of % Get application configuration variables
               {ok, Opts0} ->
                 Opts0;
               _ ->
                 []
           end,
    State = set_opts(#state{}, Opts),
    {ok, State}.

%% Handles all synchronous calls to the server. See specific functions for more
%% detailed information.

-spec handle_call({set_opts, connection_list()}, any(), any()) ->
                     {reply, ok, state()};
                 (get_opts, any(), state()) -> 
                     {reply, connection_list(), state()};
                 (stop, any(), any()) -> 
                     {stop, normal, ok, any()}.
handle_call({set_opts, Opts}, _From, State) ->
    NewState = set_opts(State, Opts),
    {reply, ok, NewState};
handle_call(get_opts, _From, #state{opts = Opts} = State) ->
    {reply, Opts, State};
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

%% Allows updating of the state by specifying a list of tuples or a single tuple
%% e.g. set_opts([username, <<"TestUser">>]).

-spec set_opts(connection_list()) -> ok | {error, badarg, binary()};
              (connection_part()) -> ok | {error, badarg, binary()}.
set_opts(Opts) when is_list(Opts) ->
    gen_server:call(?MODULE, {set_opts, Opts});
set_opts(OptTuple) when is_tuple(OptTuple) ->
    gen_server:call(?MODULE, {set_opts, [OptTuple]}).

%% Returns the connections details. Namely, username, password, host, port and
%% poll_every.

-spec get_opts() -> connection_list().
get_opts() ->
    gen_server:call(?MODULE, get_opts).
