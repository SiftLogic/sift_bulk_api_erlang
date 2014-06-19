-module(siftbulk_ftp).
-export([connect/4]).

-define(TIMEOUT, 10000).

%% Use this to easily check for issues in the server
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
-define(DO_CONNECT(Host, Port), (siftbulk_ftp:do_connect(Host, Port))).
-define(DO_LOGIN(Socket, Username, Password),
        (siftbulk_ftp:do_login(Socket, Username, Password))).
-else.
-define(DO_CONNECT(Host, Port), (do_connect(Host, Port))).
-define(DO_LOGIN(Socket, Username, Password),
        (do_login(Socket, Username, Password))).
-endif.

%% Connects to the ftp server using the passed in connection information.

-spec connect(string(), non_neg_integer(), string(), string()) -> {ok, port()}.
connect(Host, Port, User, Password) when is_integer(Port) ->
    case ?DO_CONNECT(Host, Port) of
        {ok, Socket, Message} ->
            ?DEBUG_PRINT("Connect to server ~p~n", [Message]),
            ?DO_LOGIN(Socket, User, Password);
        {error, Error} ->
            ?DEBUG_PRINT("Connect to server error ~p~n", [Error]),
            {error, Error}
    end.

do_connect(Host, Port) ->
  case gen_tcp:connect(Host, Port, [binary, {packet, line},
                                    {keepalive, true}, {active, false}]) of
    {ok, Socket} ->
      case read_possible_multiline_reply(Socket, ?TIMEOUT) of
        {ok, _Code, <<"220", Banner/binary>> = _Msg} ->
          {ok, Socket, Banner};
        _Other ->
            {error, _Other}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

do_login(Socket, Username, Password) ->
    ok = gen_tcp:send(Socket, make_command("USER", Username)),
    case read_possible_multiline_reply(Socket, ?TIMEOUT) of
        {ok, <<"331">>, MessageOuter} ->
            ?DEBUG_PRINT("user ~p~n", [MessageOuter]),
            ok = gen_tcp:send(Socket, make_command("PASS", Password)),
            case read_possible_multiline_reply(Socket, ?TIMEOUT) of
                {ok, <<"230">>, MessageInner} ->
                    ?DEBUG_PRINT("pass ~p~n", [MessageInner]),
                    {ok, Socket};
                _Other ->
                    {error, _Other}
            end;
        _Other ->
            {error, _Other}
    end.

% Transforms the command into a format that will be accepted by the ftp server.

-spec make_command(string(), string()) -> binary().
make_command(Command, Argument) ->
    iolist_to_binary([io_lib:format("~s ~s", [Command, Argument]), "\r\n"]).

-spec read_possible_multiline_reply(port(),
                                    non_neg_integer() | 'infinity') ->
                                        {ok, binary(), binary()}.
read_possible_multiline_reply(Socket, Timeout) ->
    case gen_tcp:recv(Socket, 0, Timeout) of
        {ok, <<Code:3/binary, "-", _/binary>> = Packet} ->
            read_multiline_reply(Socket, Timeout, Code, [Packet]);
        {ok, <<Code:3/binary, Sep, _/binary>> = Packet}
          when Sep =:= $\s; %% That's a space, which is expected.
               Sep =:= $\r -> %% What are standards for, right?
            {ok, Code, Packet};
        {ok, Packet} ->
            ReasonMsg = <<"Error in read_possible_multiline_reply">>,
            {error, ReasonMsg};
        {error, Reason} ->
            {error, Reason}
    end.

-spec read_multiline_reply(port(),
                           non_neg_integer() | 'infinity',
                           binary(),
                           [binary()]) ->
                               {ok, binary(), binary()}.
read_multiline_reply(Socket, Timeout, Code, Acc) ->
    case gen_tcp:recv(Socket, 0, Timeout) of
        {ok, <<Code:3/binary, " ", _/binary>> = Packet} ->
            {ok, Code, list_to_binary(lists:reverse([Packet | Acc]))};
        {ok, <<Code:3/binary, "-", _/binary>> = Packet} ->
            read_multiline_reply(Socket, Timeout, Code, [Packet | Acc]);
        {ok, Packet} ->
            {error, lists:reverse([Packet | Acc])};
        {error, Reason} ->
            {error, Reason}
    end.