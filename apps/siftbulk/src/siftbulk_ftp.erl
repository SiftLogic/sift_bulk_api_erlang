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

%% Need to use siftbulk_ftp for external calls for proper mocking with meck.
-ifdef(TEST).
-compile(export_all).
-define(DO_CONNECT(Host, Port, IsLoggedIn), (siftbulk_ftp:do_connect(Host, Port, IsLoggedIn))).
-define(DO_LOGIN_STEP1(Socket, Username, Password),
        (siftbulk_ftp:do_login_step1(Socket, Username, Password))).
-define(DO_LOGIN_STEP2, fun siftbulk_ftp:do_login_step2/2).
-define(DO_GET_PASSIVE(Socket), (siftbulk_ftp:do_get_passive(Socket))).
-define(READ_REPLY(Socket, Timeout), 
        (siftbulk_ftp:read_reply(Socket, Timeout))).
-define(READ_REPLY(Socket, Timeout, Code, Acc), 
        (siftbulk_ftp:read_reply(Socket, Timeout, Code, Acc))).
-define(READ_REPLY_CALL, fun siftbulk_ftp:read_reply_call/4).
-else.
-define(DO_CONNECT(Host, Port, IsLoggedIn), (do_connect(Host, Port, IsLoggedIn))).
-define(DO_LOGIN_STEP1(Socket, Username, Password),
        (do_login_step1(Socket, Username, Password))).
-define(DO_LOGIN_STEP2,fun do_login_step2/2).
-define(DO_GET_PASSIVE(Socket), (do_get_passive(Socket))).
-define(READ_REPLY(Socket, Timeout), 
        (read_reply(Socket, Timeout))).
-define(READ_REPLY(Socket, Timeout, Code, Acc), 
        (siftbulk_ftp:read_reply(Socket, Timeout, Code, Acc))).
-define(READ_REPLY_CALL, fun read_reply_call/4).
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
    case ?DO_CONNECT(Host, Port, false) of
        {ok, Socket, Message} ->
            ?DEBUG_PRINT("Connected to server ~p~n", [Message]),

            case ?DO_LOGIN_STEP1(Socket, User, Password) of
                {ok, _Socket} ->
                    ?DO_GET_PASSIVE(Socket);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            ?DEBUG_PRINT("Connected to server error ~p~n", [Reason]),
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
                    case ?READ_REPLY(Socket, ?TIMEOUT) of
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
    case ?READ_REPLY(Socket, ?TIMEOUT) of
        {ok, <<"331">>, MessageOuter} ->
            ?DEBUG_PRINT("user ~p~n", [MessageOuter]),
            ?DO_LOGIN_STEP2(Socket, Password);
        _Other ->
            {error, _Other}
    end.

%% I need to do this because otherwise I would need to return different values 
%% based on the exact same arguments to ?READ_REPLY which is impossible.

-spec do_login_step2(port(), string()) -> {ok, port()} | {error, any()}.
do_login_step2(Socket, Password) ->
    ok = gen_tcp:send(Socket, make_command("PASS", Password)),
    case ?READ_REPLY(Socket, ?TIMEOUT) of
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
    case ?READ_REPLY(Socket, ?TIMEOUT) of
        {ok, <<"227">>, MessageInner} ->
            Parts = binary:split(MessageInner, [<<"(">>, <<")">>], [global]),
            IPAndPort = binary:split(lists:nth(2, Parts), <<",">>, [global]),

            %% Gen_tcp accepts deep lists (WTF erlang?:( )
            String1 = binary_to_list(lists:nth(1, IPAndPort)),
            String2 = binary_to_list(lists:nth(2, IPAndPort)),
            String3 = binary_to_list(lists:nth(3, IPAndPort)),
            String4 = binary_to_list(lists:nth(4, IPAndPort)),
            Host = string:join([String1, String2, String3, String4], "."),

            {Port1, Port2} = {lists:nth(5, IPAndPort), lists:nth(6, IPAndPort)},
            Port = calculate_port(Port1, Port2),

            case ?DO_CONNECT(Host, Port, true) of
                {ok, PassiveSocket} ->
                    {ok, Socket, PassiveSocket};
                {error, Reason} ->
                    {error, Reason}
            end;
        _Other ->
            {error, _Other}
    end.

%% Takes 2 binary parts and returns the port number as an integer.

-spec calculate_port(binary(), binary()) -> integer().
calculate_port(Port1, Port2) ->
    X = list_to_integer(binary_to_list(Port1)),
    Y = list_to_integer(binary_to_list(Port2)),
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
            ?READ_REPLY_CALL(Socket, Timeout, Code, [Packet | Acc]);
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

%% So read_reply can be stubbed without causing infinite recursion.

-spec read_reply_call(port(),
                 non_neg_integer() | 'infinity',
                 binary() | undefined,
                 [binary()]) ->
                     {ok, binary(), binary()}.
read_reply_call(Socket, Timeout, PreviousCode, Acc) ->
    read_reply(Socket, Timeout, PreviousCode, Acc).
