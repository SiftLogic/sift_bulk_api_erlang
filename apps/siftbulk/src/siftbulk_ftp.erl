-module(siftbulk_ftp).

-export([connect/4]).

-define(TIMEOUT, 10000).

%% Use this to easily check for issues in the server.
-define(DEBUG, true).
-ifdef(DEBUG).
-define(DEBUG_PRINT(Format, Values),
        io:format(Format, Values)).
-else.
-define(DEBUG_PRINT(Format, Values), ok).
-endif.

-ifdef(TEST).
-compile(export_all).
-endif.

%% Takes the local file and uploads it to the remote file location.

% upload(Socket, LocalFile, RemoteFile) ->
%     RemoteFileName = filename:basename(RemoteFile),

%     ok = gen_tcp:send(Socket, make_command("CWD",filename:dirname(RemoteFile))),
%     case ?READ_REPLY(Socket, ?TIMEOUT) of
%         {ok, _Code, <<"250", Message/binary>> = _Msg} ->
%             ?DEBUG_PRINT("Change server directory to ~p~n", [Message]),

%             % ?DEBUG_PRINT("cmd: PASV ~p ~p~n", [LocalFile, RemoteFileName]),
%             % ok = gen_tcp:send(Socket, make_command("STOR", LocalFile ++ " " ++ RemoteFileName)),
%             % case ?READ_REPLY(Socket, ?TIMEOUT) of
%             %     _Other->
%             %         ?DEBUG_PRINT("Upload to ~p~n", [RemoteFile]),
%             %         ok = gen_tcp:send(Socket, make_command("STOR", LocalFile ++ " " ++ RemoteFileName)),
%             %         case ?READ_REPLY(Socket, ?TIMEOUT) of
%             %             _Other->
%             %                 ?DEBUG_PRINT("Upload to ~p~n", [RemoteFile]),
%             %                 {error, _Other}
%             %         end;
%             % end;
%         _Other ->
%             {error, _Other}
%     end.
%     % ok = gen_tcp:send(Socket, make_command("STOR", File)),

%% Logins to the ftp server using the passed in connection information.
%% Note: Also, starts a passive mode connection.

-spec connect(string(), non_neg_integer(), string(), string()) -> 
    {ok, port(), port()} | {error, any()}.
connect(Host, Port, User, Password) when is_integer(Port) ->
    case do_connect(Host, Port, false) of
        {ok, Socket, Message} ->
            ?DEBUG_PRINT("Connected to server ~p~n", [Message]),

            case do_login_step1(Socket, User, Password) of
                {ok, _Socket} ->
                    do_get_passive(Socket);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            ?DEBUG_PRINT("Connecting to server error ~p~n", [Reason]),
            {error, Reason}
    end.

%% Connects and waits for a response expects a banner and parses it if not logged in. If logged it
%% just returns the reply.

-spec do_connect(string(), non_neg_integer(), boolean()) -> 
    {ok, port(), string()} | {error, any()}.
do_connect(Host, Port, IsLoggedIn) ->
    case IsLoggedIn of
        true ->
            gen_tcp:connect(Host, Port, [binary, {packet, line},
                                         {keepalive, true}, {active, true}]);
        false ->
            case gen_tcp:connect(Host, Port, [binary, {packet, line},
                                              {keepalive, true}, {active, false}]) of
                {ok, Socket} ->
                    case read_reply(Socket, ?TIMEOUT) of
                        {ok, _Code, <<"220", Banner/binary>> = _Msg} ->
                            {ok, Socket, Banner};
                        _Other ->
                            {error, _Other}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec do_login_step1(port(), string(), string()) -> {ok, port()} | {error, any()}.
do_login_step1(Socket, Username, Password) ->
    ok = gen_tcp:send(Socket, make_command("USER", Username)),
    case read_reply(Socket, ?TIMEOUT) of
        {ok, <<"331">>, MessageOuter} ->
            ?DEBUG_PRINT("user ~p~n", [MessageOuter]),
            do_login_step2(Socket, Password);
        _Other ->
            {error, _Other}
    end.

%% I need to do this because otherwise I would need to return different values 
%% based on the exact same arguments to ?READ_REPLY which is impossible.

-spec do_login_step2(port(), string()) -> {ok, port()} | {error, any()}.
do_login_step2(Socket, Password) ->
    ok = gen_tcp:send(Socket, make_command("PASS", Password)),
    case read_reply(Socket, ?TIMEOUT) of
        {ok, <<"230">>, MessageInner} ->
            ?DEBUG_PRINT("pass ~p~n", [MessageInner]),
            {ok, Socket};
        _Other ->
            {error, _Other}
    end.

%% Gets the new port and IP address to connect from a PASV command.

-spec do_get_passive(port()) -> {ok, port(), port()} | {error, any()}.
do_get_passive(Socket) ->
    ok = gen_tcp:send(Socket, make_command("PASV", "")),
    case read_reply(Socket, ?TIMEOUT) of
        {ok, <<"227">>, MessageInner} ->
            [_, PortMap, _] = binary:split(MessageInner, [<<"(">>, <<")">>], [global]),
            [S1, S2, S3, S4, P1, P2] =
                [binary_to_list(P) || P <- binary:split(
                                             PortMap,
                                             <<",">>,
                                             [global])],
            Host = string:join([S1, S2, S3, S4], "."),

            Port = calculate_port(P1, P2),

            case do_connect(Host, Port, true) of
                {ok, PassiveSocket} ->
                    {ok, Socket, PassiveSocket};
                {error, Reason} ->
                    {error, Reason}
            end;
        _Other ->
            {error, _Other}
    end.

%% Takes 2 binary parts and returns the port number as an integer.

-spec calculate_port(list(), list()) -> integer().
calculate_port(Port1, Port2) ->
    X = list_to_integer(Port1),
    Y = list_to_integer(Port2),
    (X * 256) + Y.

%% Transforms the command into a format that will be accepted by the ftp server.

-spec make_command(string(), string()) -> binary().
make_command(Command, Argument) ->
    iolist_to_binary([io_lib:format("~s ~s", [Command, Argument]), "\r\n"]).

%% Assemble the multiple lines of packets.

-spec read_reply(port(),
                 non_neg_integer() | 'infinity',
                 binary() | undefined,
                 [binary()]) ->
                     {ok, binary(), binary()}.
read_reply(Socket, Timeout) ->
    read_reply(Socket, Timeout, undefined, []).

read_reply(Socket, Timeout, PreviousCode, Acc) ->
    case gen_tcp:recv(Socket, 0, Timeout) of
        {ok, <<Code:3/binary, "-", _/binary>> = Packet} 
          when PreviousCode =:= undefined;
               PreviousCode =:= Code ->
            read_reply(Socket, Timeout, Code, [Packet | Acc]);
        {ok, <<Code:3/binary, Sep, _/binary>> = Packet}
          when (Sep =:= $\s orelse %% That's a space, which is expected.
                Sep =:= $\r) andalso 
               (PreviousCode =:= undefined orelse
                PreviousCode =:= Code) -> %% What are standards for, right?
            {ok, Code, list_to_binary(lists:reverse([Packet | Acc]))};
        {ok, Packet} ->
            {error, lists:reverse([Packet | Acc])};
        {error, Reason} ->
            {error, Reason}
    end.
