%% @doc Contains all the operations to upload, poll and download files.

-module(siftbulk_client).
-behaviour(gen_server).

% Stores connection information visible to users of the ftp server.
-record(state, {opts = [{username, undefined},
                        {password, undefined},
                        {host, "localhost"},
                        {port, 21},
                        {poll_every, 300}],
                connection = undefined,
                data = undefined}).

-export([start_link/0, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([set_opts/2, get_opts/1, init/1, connect/0, stop/0]).

-type connection_part() :: {atom(), any()}.
-type connection_list() :: list(connection_part()).

%% Private

-spec set_opts_(#state{}, connection_list()) -> #state{}.
set_opts_(#state{} = State, []) ->
    State;
set_opts_(#state{} = State, Opts) ->
    Fields = [host, password, poll_every, port, username],
    set_opts_(State, Fields, Opts).

-spec set_opts_(#state{}, list(), connection_list()) -> #state{}.
set_opts_(#state{} = State, [], _Opts) ->
    State;
set_opts_(#state{opts = OldOpts} = State, [Field | Tail], Opts) ->
    case lists:keyfind(Field, 1, Opts) of
        {Field, _} = NewValue ->
            case lists:keyfind(Field, 1, OldOpts) of
                false -> 
                    set_opts_(
                        State#state{opts = [NewValue | OldOpts]},
                        Tail, Opts);
                _ ->
                    set_opts_(
                        State#state{opts = lists:keyreplace(Field, 1, OldOpts, NewValue)},
                        Tail, Opts)
            end;
        false ->
            set_opts_(State, Tail, Opts)
    end.

-spec connect(connection_list()) -> 
    {ok, port()} | {error, any()}.
connect(Opts) ->
    siftbulk_ftp:connect(proplists:get_value(host, Opts),
                         proplists:get_value(port, Opts),
                         proplists:get_value(username, Opts),
                         proplists:get_value(password, Opts)).

% upload(File, Opts) ->
%     io:format("uploading the file: ~p~n", File),
%     ok.

%% Starts the API server.

-spec start_link() -> {atom(), pid()}.
start_link() ->
    gen_server:start_link(?MODULE, ?MODULE, []).

%% Initializes the API server with FTP connection options, retrieved from
%% sys.config:
%% user: The username to get into the ftp server.
%% password: The password to get into the ftp server.
%% host: The host to connect to. Defaults to localhost.
%% port: The port to connect to. Defaults to 21.

-spec init([]) -> {ok, #state{}}.
init(_) ->
    Opts = case application:get_env(siftbulk, auth) of % Get application configuration variables
               {ok, Opts0} ->
                 Opts0;
               _ ->
                 []
           end,

    State = set_opts_(#state{}, Opts),

    {ok, State}.

%% Uses the passed in configuration options to connect to the server.

-spec connect() -> {atom(), pid()} | {error, atom()}.
connect() ->
    gen_server:call(?MODULE, connect).

% Changes to the upload directory then uploads the specified file.
%
% upload(File) ->
%     gen_server:call(?MODULE, {upload, File}).

%% Handles all synchronous calls to the server. See specific functions for more
%% detailed information.

-spec handle_call({set_opts_, connection_list()}, any(), any()) ->
                     {reply, ok, #state{}};
                 (get_opts, any(), #state{}) -> 
                     {reply, connection_list(), #state{}};
                 (connect, any(), #state{}) -> 
                     {reply, {ok, port()} | {error, any()}, #state{}};
                 (stop, any(), any()) -> 
                     {stop, normal, ok, any()}.
handle_call({set_opts_, Opts}, _From, State) ->
    NewState = set_opts_(State, Opts),
    {reply, ok, NewState};
handle_call(get_opts, _From, #state{opts = Opts} = State) ->
    % io:format("In handle call~n"),
    {reply, Opts, State};
handle_call(connect, _From, #state{opts = Opts} = State) ->
    case connect(Opts) of
        {ok, Pid, PassivePid} ->
            {reply, ok, State#state{connection = Pid, data = PassivePid}};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;
% handle_call({upload, File}, _From, #state{opts = Opts} = State) ->
%     ok = upload(File, Opts),
%     {reply, ok, State};
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

-spec set_opts(pid(), connection_list()) -> ok | {error, badarg, binary()};
              (pid(), connection_part()) -> ok | {error, badarg, binary()}.
set_opts(Pid, Opts) when is_list(Opts) ->
    gen_server:call(Pid, {set_opts_, Opts});
set_opts(Pid, OptTuple) when is_tuple(OptTuple) ->
    gen_server:call(Pid, {set_opts_, [OptTuple]}).

%% Returns the connections details. Namely, username, password, host, port and
%% poll_every.

-spec get_opts(pid()) -> connection_list().
get_opts(Pid) ->
    gen_server:call(Pid, get_opts).
