-module(siftbulk_ftp_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("tests.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This code, due to the ftp dependency, has a lot of side effects. So privates
%% will be tested.
make_command_test_() ->
    {"Verifies that the passed in are converted to a correct format and it is
      in binary form",
     ?setup([fun test_make_command_empty/0,
             fun test_make_command_empty_argument/0,
             fun test_make_command_empty_command/0,
             fun test_make_command_filled_in/0])}.

calculate_port_test_() ->
    {"Verifies that the correct port integer is returned",
     ?setup([fun test_calculate_port_zeroes/0,
             fun test_calculate_port_normal/0])}.

read_reply_test_() ->
    {"Verifies a line from a response is read in the correct way",
     ?setup_mock([fun test_read_reply_failure/0,
                  fun test_read_reply_failure_on_ok/0,
                  fun test_read_reply_success_not_last_line/0,
                  fun test_read_reply_success_last_line/0])}.

do_connect_test_() ->
    {"Verifies it reads the reply and returns the banner or sends back the 
      right error",
     ?setup_mock([fun test_do_connect_failure_no_login/0,
                  fun test_do_connect_success_multiline_failure_no_login/0,
                  fun test_do_connect_success_multiline_success_no_login/0,
                  fun test_do_connect_success_multiline_success_logged_in/0])}.

% do_login_step1_test_() ->
%     {"Verifies that a USER command is either parsed or handled via relevant
%       error messages",
%      ?setup_mock([fun test_do_login_step1_multiline_failure/0,
%                   fun test_do_login_step1_multiline_success/0])}.

% do_login_step2_test_() ->
%     {"Verifies that a PASS command is either parsed or handled via relevant
%       error messages",
%      ?setup_mock([fun test_do_login_step2_multiline_failure/0,
%                   fun test_do_login_step2_multiline_success/0])}.

% do_get_passive_test_() ->
%     {"Verifies that a PASV command is made and a socket it retrieved using
%       returned connection details",
%      ?setup_mock([fun test_do_get_passive_failure/0,
%                   fun test_do_get_passive_do_connect_failure/0,
%                   fun test_do_get_passive_success/0])}.

% connect_test_() ->
%     {"Verifies it returns the result of a login or sends back an error message",
%      ?setup_mock([fun test_connect_failure/0,
%                   fun test_connect_success_login_failure/0,
%                   fun test_connect_success_login_success/0])}.

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    ok = application:start(siftbulk).
 
stop(_) ->
    ok = quiet_stop(siftbulk).

start_mock() ->
    start(),
    meck:new(siftbulk_ftp, [unstick]),
    meck:new(gen_tcp, [unstick]).
 
stop_mock(_Arg) ->
    meck:validate(siftbulk_ftp),
    meck:unload(siftbulk_ftp),
    meck:validate(gen_tcp),
    meck:unload(gen_tcp),
    ok = stop(_Arg).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

%% make command

test_make_command_empty() ->
    ?assertEqual(<<" \r\n">>, siftbulk_ftp:make_command("", "")).

test_make_command_empty_argument() ->
    ?assertEqual(<<"RECV \r\n">>, siftbulk_ftp:make_command("RECV", "")).

test_make_command_empty_command() ->
    ?assertEqual(<<" test\r\n">>, siftbulk_ftp:make_command("", "test")).

test_make_command_filled_in() ->
    ?assertEqual(<<"RECV test.csv\r\n">>,
                 siftbulk_ftp:make_command("RECV", "test.csv")).

%% calculate_port

test_calculate_port_zeroes() ->
    ?assertEqual(0, siftbulk_ftp:calculate_port(<<"0">>, <<"0">>)).

test_calculate_port_normal() ->
    ?assertEqual(25601, siftbulk_ftp:calculate_port(<<"100">>, <<"1">>)).

%% read_reply

test_read_reply_failure() ->
    meck:expect(siftbulk_ftp, read_reply, 
                fun(String, Integer, Binary, List) ->
                    meck:passthrough([String, Integer, Binary, List]) end),
    meck:expect(gen_tcp, recv, fun(_, _, _) -> {error, "An Error"} end),

    Result = siftbulk_ftp:read_reply("Socket", 10, <<"230">>, []),

    ?assertEqual(Result, {error, "An Error"}).

test_read_reply_failure_on_ok() ->
    meck:expect(siftbulk_ftp, read_reply, 
                fun(String, Integer, Binary, List) ->
                    meck:passthrough([String, Integer, Binary, List]) end),
    meck:expect(gen_tcp, recv, fun(_, _, _) -> {ok, "A message"} end),

    Result = siftbulk_ftp:read_reply("Socket", 10, <<"230">>, 
                                               ["One", "Two"]),

    ?assertEqual(Result, {error, ["Two", "One", "A message"]}),

    %% A non matching code
    meck:expect(gen_tcp, recv, fun(_, _, _) -> {ok, <<"220 Test">>} end),

    Result1 = siftbulk_ftp:read_reply("Socket", 10, <<"230">>, 
                                               ["One", "Two"]),

    ?assertEqual(Result1, {error, ["Two", "One", <<"220 Test">>]}).

test_read_reply_success_not_last_line() ->
    ReturnTuple = {ok, <<"220">>, <<"TwoOne220 Part1-221 Part2">>},
    meck:expect(siftbulk_ftp, read_reply, 
                fun(String, Integer, Binary, List) ->
                    meck:passthrough([String, Integer, Binary, List]) end),
    meck:expect(gen_tcp, recv,
        fun(_, _, _) -> {ok, <<"220 Part1-221 Part2">>} end),

    Result = siftbulk_ftp:read_reply("Socket", 10, undefined, ["One", "Two"]),

    ?assertEqual(Result, ReturnTuple).

test_read_reply_success_last_line() ->
    meck:expect(siftbulk_ftp, read_reply, 
                fun(String, Integer, Binary, List) ->
                    meck:passthrough([String, Integer, Binary, List]) end),
    meck:expect(gen_tcp, recv,
        fun(_, _, _) -> {ok, <<"220 Test">>} end),

    Result = siftbulk_ftp:read_reply("Socket", 10, undefined, ["One", "Two"]),

    ?assertEqual(Result, {ok, <<"220">>, <<"TwoOne220 Test">>}),

    meck:expect(gen_tcp, recv,
        fun(_, _, _) -> {ok, <<"220\rTest">>} end),

    Result1 = siftbulk_ftp:read_reply("Socket", 10, undefined, ["One", "Two"]),

    ?assertEqual(Result1, {ok, <<"220">>, <<"TwoOne220\rTest">>}),

    meck:expect(gen_tcp, recv,
        fun(_, _, _) -> {ok, <<"220 Test">>} end),

    Result2 = siftbulk_ftp:read_reply("Socket", 10, <<"220">>, ["One", "Two"]),
    ?assertEqual(Result2, {ok, <<"220">>, <<"TwoOne220 Test">>}),

    meck:expect(gen_tcp, recv,
        fun(_, _, _) -> {ok, <<"220\rTest">>} end),

    Result3 = siftbulk_ftp:read_reply("Socket", 10, <<"220">>, ["One", "Two"]),
    ?assertEqual(Result3, {ok, <<"220">>, <<"TwoOne220\rTest">>}).

%% do_connect

test_do_connect_failure_no_login() ->
    meck:expect(siftbulk_ftp, do_connect, fun(Arg1, Arg2, Arg3) ->
                                              meck:passthrough([Arg1,
                                                                Arg2,
                                                                Arg3]) end),
    meck:expect(gen_tcp, connect, fun(_, _, _) -> 
                                      {error, "An Error"} end),

    Result = siftbulk_ftp:do_connect("localhost", 21, false),

    ?assert(meck:called(gen_tcp, connect,["localhost", 21, [binary,
                                                            {packet, line},
                                                            {keepalive, true},
                                                            {active, false}]])),
    ?assertEqual(Result, {error, "An Error"}).

test_do_connect_success_multiline_failure_no_login() ->
    %% Socket does not need to be a socket object because do_login_step1 is
    %% mocked
    meck:expect(siftbulk_ftp, do_connect, fun(Arg1, Arg2, Arg3) ->
                                              meck:passthrough([Arg1,
                                                                Arg2,
                                                                Arg3]) end),
    meck:expect(gen_tcp, connect, fun(_, _, _) -> {ok, "Socket"} end),
    meck:expect(gen_tcp, recv,
        fun(_, _, _) -> {error, "Error"} end),

    Result = siftbulk_ftp:do_connect("localhost", 21, false),
    
    ?assertEqual(Result, {error, {error, "Error"}}).

test_do_connect_success_multiline_success_no_login() ->
    Socket = "Socket",
    meck:expect(siftbulk_ftp, do_connect, fun(Arg1, Arg2, Arg3) ->
                                              meck:passthrough([Arg1,
                                                                Arg2,
                                                                Arg3]) end),
    meck:expect(gen_tcp, connect, fun(_, _, _) -> {ok, Socket} end),
    meck:expect(gen_tcp, recv,
        fun(_, _, _) -> {ok, <<"220 The banner">>} end),

    Result = siftbulk_ftp:do_connect("localhost", 21, false),
    
    ?assertEqual(Result, {ok, Socket, <<" The banner">>}).

test_do_connect_success_multiline_success_logged_in() ->
    meck:expect(siftbulk_ftp, do_connect, fun(Arg1, Arg2, Arg3) ->
                                              meck:passthrough([Arg1,
                                                                Arg2,
                                                                Arg3]) end),
    meck:expect(gen_tcp, connect, fun(_, _, _) -> {ok, "A Message"} end),

    Result = siftbulk_ftp:do_connect("localhost", 21, true),
    
    ?assertEqual(Result, {ok, "A Message"}).

% %% do_login_step1

% test_do_login_step1_multiline_failure() ->
%     Socket = "Socket",
%     meck:expect(siftbulk_ftp, do_login_step1, fun(_, Arg2, Arg3) ->
%                                               meck:passthrough([Socket, Arg2,
%                                                                 Arg3]) end),
%     meck:expect(gen_tcp, send, fun(_, _) -> ok end),
%     meck:expect(siftbulk_ftp, read_reply, 
%                 fun(_, _) -> {ok, <<"332">>, "Test"} end),

%     Result = siftbulk_ftp:do_login_step1(Socket, "TestKey", "123fd-4"),
    
%     ?assert(meck:called(gen_tcp, send, [Socket, <<"USER TestKey\r\n">>])),
%     ?assert(meck:called(siftbulk_ftp, read_reply,
%                         [Socket, 10000])),
%     ?assertEqual(Result, {error, {ok, <<"332">>, "Test"}}).

% test_do_login_step1_multiline_success() ->
%     Socket = "Socket",
%     meck:expect(siftbulk_ftp, do_login_step1, fun(_, Arg2, Arg3) ->
%                                             meck:passthrough([Socket, Arg2,
%                                                               Arg3]) end),
%     meck:expect(siftbulk_ftp, do_login_step2, 
%                 fun(_, _) -> {ok, "Socket"} end),
%     meck:expect(gen_tcp, send, fun(_, _) -> ok end),

%     meck:expect(siftbulk_ftp, read_reply, 
%                 fun(_, _) -> {ok, <<"331">>, "Test"} end),

%     Result = siftbulk_ftp:do_login_step1(Socket, "TestKey", "123fd-4"),
    
%     ?assert(meck:called(siftbulk_ftp, read_reply,
%                         [Socket, 10000])),
%     ?assertEqual(Result, {ok, "Socket"}).

% %% do_login_step2

% test_do_login_step2_multiline_failure() ->
%     Socket = "Socket",
%     meck:expect(siftbulk_ftp, do_login_step2, fun(_, Arg2) ->
%                                                   meck:passthrough([Socket, 
%                                                                     Arg2]) end),
%     meck:expect(gen_tcp, send, fun(_, _) -> ok end),
%     meck:expect(siftbulk_ftp, read_reply, 
%                 fun(_, _) -> {ok, <<"231">>, "Test"} end),

%     Result = siftbulk_ftp:do_login_step2(Socket, "123fd-4"),
    
%     ?assert(meck:called(gen_tcp, send, [Socket, <<"PASS 123fd-4\r\n">>])),
%     ?assert(meck:called(siftbulk_ftp, read_reply,
%                         [Socket, 10000])),
%     ?assertEqual(Result, {error, {ok, <<"231">>, "Test"}}).

% test_do_login_step2_multiline_success() ->
%     Socket = "Socket",
%     meck:expect(siftbulk_ftp, do_login_step2, fun(_, Arg2) ->
%                                                   meck:passthrough([Socket, 
%                                                                     Arg2]) end),
%     meck:expect(gen_tcp, send, fun(_, _) -> ok end),
%     meck:expect(siftbulk_ftp, read_reply, 
%                 fun(_, _) -> {ok, <<"230">>, "Test"} end),

%     Result = siftbulk_ftp:do_login_step2(Socket, "123fd-4"),
    
%     ?assert(meck:called(gen_tcp, send, [Socket, <<"PASS 123fd-4\r\n">>])),
%     ?assert(meck:called(siftbulk_ftp, read_reply,
%                         [Socket, 10000])),
%     ?assertEqual(Result, {ok, Socket}).

% %% do_get_passive

% test_do_get_passive_failure() ->
%     Socket = "Socket",
%     Response = <<"221 Not the right code.\r\n">>,

%     meck:expect(siftbulk_ftp, do_get_passive, 
%                 fun(_) -> meck:passthrough([Socket]) end),

%     meck:expect(gen_tcp, send, fun(_, _) -> ok end),
%     meck:expect(siftbulk_ftp, read_reply,
%                 fun(_, _) -> {error, Response} end),

%     Result = siftbulk_ftp:do_get_passive(Socket),

%     ?assertEqual(Result, {error, {error, Response}}).

% test_do_get_passive_do_connect_failure() ->
%     Socket = "Socket",

%     meck:expect(siftbulk_ftp, do_get_passive, 
%                 fun(_) -> meck:passthrough([Socket]) end),

%     meck:expect(gen_tcp, send, fun(_, _) -> ok end),
%     meck:expect(siftbulk_ftp, read_reply,
%                 fun(_, _) -> {ok, <<"227">>,
%                               <<"227 Entering Passive Mode (127,0,0,1,229,76).\r\n">>} end),

%     meck:expect(siftbulk_ftp, do_connect,
%                 fun(_, _, _) -> {error, "An Error"} end),

%     Result = siftbulk_ftp:do_get_passive(Socket),

%     ?assertEqual(Result, {error, "An Error"}).

% test_do_get_passive_success() ->
%     Socket = "Socket",
%     PassiveSocket = "Passive Socket",

%     meck:expect(siftbulk_ftp, do_get_passive, 
%                 fun(_) -> meck:passthrough([Socket]) end),

%     meck:expect(gen_tcp, send, fun(_, _) -> ok end),
%     meck:expect(siftbulk_ftp, read_reply,
%                 fun(_, _) -> {ok, <<"227">>,
%                               <<"227 Entering Passive Mode (127,0,0,1,229,76).\r\n">>} end),

%     meck:expect(siftbulk_ftp, do_connect,
%                 fun(_, _, _) -> {ok, PassiveSocket} end),

%     Result = siftbulk_ftp:do_get_passive(Socket),

%     ?assert(meck:called(siftbulk_ftp, do_connect, ["127.0.0.1", 58700, true])),

%     ?assertEqual(Result, {ok, Socket, PassiveSocket}).

% %% connect

% test_connect_failure() ->
%     %% Socket does not need to be a socket object because do_login is mocked
%     meck:expect(siftbulk_ftp, connect, fun(Arg1, Arg2, Arg3, Arg4) ->
%                                             meck:passthrough([Arg1, Arg2, Arg3,
%                                                               Arg4]) end),
%     meck:expect(siftbulk_ftp, do_connect, fun(_, _, _) -> {error, "An Error"} end),

%     Result = siftbulk_ftp:connect("localhost", 21, "TestKey", "123fd-4"),

%     ?assert(meck:called(siftbulk_ftp, do_connect, ["localhost", 21, false])),
%     ?assertEqual(Result, {error, "An Error"}).

% test_connect_success_login_failure() ->
%     meck:expect(siftbulk_ftp, connect, fun(Arg1, Arg2, Arg3, Arg4) ->
%                                             meck:passthrough([Arg1, Arg2, Arg3,
%                                                               Arg4]) end),
%     meck:expect(siftbulk_ftp, do_connect, fun(_, _, _) -> 
%                                               {ok, "Socket", "A Message"} end),
%     meck:expect(siftbulk_ftp, do_login_step1, fun(_, _, _) -> 
%                                                   {error, "An Error"} end),

%     Result = siftbulk_ftp:connect("localhost", 21, "TestKey", "123fd-4"),

%     ?assert(meck:called(siftbulk_ftp, do_connect, ["localhost", 21, false])),
%     ?assertEqual(Result, {error, "An Error"}).

% test_connect_success_login_success() ->
%     PassiveSocket = "PassiveSocket",
%     Socket = "Socket",

%     meck:expect(siftbulk_ftp, connect, fun(Arg1, Arg2, Arg3, Arg4) ->
%                                             meck:passthrough([Arg1, Arg2, Arg3,
%                                                               Arg4]) end),
%     meck:expect(siftbulk_ftp, do_connect, fun(_, _, _) -> 
%                                               {ok, Socket, "A Message"} end),
%     meck:expect(siftbulk_ftp, do_login_step1, fun(SocketI, _, _) -> 
%                                                   {ok, SocketI} end),
%     meck:expect(siftbulk_ftp, do_get_passive, fun(SocketI) -> 
%                                                   {ok, SocketI, PassiveSocket} end),

%     Result = siftbulk_ftp:connect("localhost", 21, "TestKey", "123fd-4"),

%     ?assert(meck:called(siftbulk_ftp, do_connect, ["localhost", 21, false])),
%     ?assertEqual(Result, {ok, Socket, PassiveSocket}).
