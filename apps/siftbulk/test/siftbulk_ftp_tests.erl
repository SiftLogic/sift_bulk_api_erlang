-module(siftbulk_ftp_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("tests.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
% This code, due to the ftp dependency, has a lot of side effects
make_command_test_() ->
    {"Verifies that the passed in are converted to a correct format and it is
      in binary form",
     ?setup([fun test_make_command_empty/0,
             fun test_make_command_empty_argument/0,
             fun test_make_command_empty_command/0,
             fun test_make_command_filled_in/0])}.

read_reply_failure_test_() ->
    {"Verifies a line from a response is read in the correct way",
     ?setup_mock([fun test_read_reply_failure/0,
                  fun test_read_reply_failure_on_ok/0,
                  fun test_read_reply_success_not_last_line/0,
                  fun test_read_reply_success_last_line/0])}.

% read_possible_multiline_reply_test_()->
%     {"Verifies a response can be read if it is multi or single line or it will
%       give specific error messages",
%      ?setup_mock([fun test_read_possible_multiline_reply_failure/0,
%                   fun test_read_possible_multiline_reply_failure_on_ok/0,
%                   fun test_read_possible_multiline_reply_success_single/0])}.

do_connect_test_() ->
    {"Verifies it reads the reply and returns the banner or sends back the 
      right error",
     ?setup_mock([fun test_do_connect_failure/0,
                  fun test_do_connect_success_multiline_failure/0,
                  fun test_do_connect_success_multiline_success/0])}.

do_login_step1_test_() ->
    {"Verifies that a USER command is either parsed or handled via relevant
      error messages",
     ?setup_mock([fun test_do_login_step1_multiline_failure/0,
                  fun test_do_login_step1_multiline_success/0])}.

do_login_step2_test_() ->
    {"Verifies that a PASS command is either parsed or handled via relevant
      error messages",
     ?setup_mock([fun test_do_login_step2_multiline_failure/0,
                  fun test_do_login_step2_multiline_success/0])}.

connect_test_() ->
    {"Verifies it returns the result of a login or sends back an error message",
     ?setup_mock([fun test_connect_failure/0,
                  fun test_connect_success/0])}.

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
test_make_command_empty() ->
    ?assertEqual(<<" \r\n">>, siftbulk_ftp:make_command("", "")).

test_make_command_empty_argument() ->
    ?assertEqual(<<"RECV \r\n">>, siftbulk_ftp:make_command("RECV", "")).

test_make_command_empty_command() ->
    ?assertEqual(<<" test\r\n">>, siftbulk_ftp:make_command("", "test")).

test_make_command_filled_in() ->
    ?assertEqual(<<"RECV test.csv\r\n">>,
                 siftbulk_ftp:make_command("RECV", "test.csv")).

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
    ReturnTuple = {ok, <<"220">>, <<"TwoOne220-Test">>},
    meck:expect(siftbulk_ftp, read_reply, 
                fun(String, Integer, Binary, List) ->
                    meck:passthrough([String, Integer, Binary, List]) end),
    meck:expect(siftbulk_ftp, read_reply_call, 
                fun(_, _, _, _) -> ReturnTuple end),
    meck:expect(gen_tcp, recv,
        fun(_, _, _) -> {ok, <<"220-Test">>} end),

    Result = siftbulk_ftp:read_reply("Socket", 10, undefined, ["One", "Two"]),


    Accumulator = [<<"220-Test">>, "One", "Two"],
    ?assert(meck:called(siftbulk_ftp, read_reply_call, 
                        ["Socket", 10, <<"220">>, Accumulator])),
    ?assertEqual(Result, ReturnTuple),

    meck:expect(gen_tcp, recv,
        fun(_, _, _) -> {ok, <<"220-Test">>} end),

    Result1 = siftbulk_ftp:read_reply("Socket", 10, <<"220">>, Accumulator),

    ?assertEqual(Result1, ReturnTuple).

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

test_do_connect_failure() ->
    meck:expect(siftbulk_ftp, do_connect, fun(Arg1, Arg2) ->
                                              meck:passthrough([Arg1,
                                                                Arg2]) end),
    meck:expect(gen_tcp, connect, fun(_, _, _) -> 
                                      {error, "An Error"} end),

    Result = siftbulk_ftp:do_connect("localhost", 21),

    ?assert(meck:called(gen_tcp, connect,["localhost", 21, [binary,
                                                            {packet, line},
                                                            {keepalive, true},
                                                            {active, false}]])),
    ?assertEqual(Result, {error, "An Error"}).

test_do_connect_success_multiline_failure() ->
    %% Socket does not need to be a socket object because do_login_step1 is
    %% mocked
    meck:expect(siftbulk_ftp, do_connect, fun(Arg1, Arg2) ->
                                              meck:passthrough([Arg1,
                                                                Arg2]) end),
    meck:expect(gen_tcp, connect, fun(_, _, _) -> {ok, "Socket"} end),
    meck:expect(siftbulk_ftp, read_reply, 
                fun(_, _) -> "Error" end),

    Result = siftbulk_ftp:do_connect("localhost", 21),
    
    ?assert(meck:called(siftbulk_ftp, read_reply,
                        ["Socket", 10000])),
    ?assertEqual(Result, {error, "Error"}).

test_do_connect_success_multiline_success() ->
    Socket = "Socket",
    meck:expect(siftbulk_ftp, do_connect, fun(Arg1, Arg2) ->
                                              meck:passthrough([Arg1,
                                                                Arg2]) end),
    meck:expect(gen_tcp, connect, fun(_, _, _) -> {ok, Socket} end),
    meck:expect(siftbulk_ftp, read_reply, 
                fun(_, _) -> {ok, Socket, <<"220 The banner">>} end),

    Result = siftbulk_ftp:do_connect("localhost", 21),
    
    ?assertEqual(Result, {ok, Socket, <<" The banner">>}).

%% do_login_step1

test_do_login_step1_multiline_failure() ->
    Socket = "Socket",
    meck:expect(siftbulk_ftp, do_login_step1, fun(_, Arg2, Arg3) ->
                                              meck:passthrough([Socket, Arg2,
                                                                Arg3]) end),
    meck:expect(gen_tcp, send, fun(_, _) -> ok end),
    meck:expect(siftbulk_ftp, read_reply, 
                fun(_, _) -> {ok, <<"332">>, "Test"} end),

    Result = siftbulk_ftp:do_login_step1(Socket, "TestKey", "123fd-4"),
    
    ?assert(meck:called(gen_tcp, send, [Socket, <<"USER TestKey\r\n">>])),
    ?assert(meck:called(siftbulk_ftp, read_reply,
                        [Socket, 10000])),
    ?assertEqual(Result, {error, {ok, <<"332">>, "Test"}}).

test_do_login_step1_multiline_success() ->
    Socket = "Socket",
    meck:expect(siftbulk_ftp, do_login_step1, fun(_, Arg2, Arg3) ->
                                            meck:passthrough([Socket, Arg2,
                                                              Arg3]) end),
    meck:expect(siftbulk_ftp, do_login_step2, 
                fun(_, _) -> {ok, "Socket"} end),
    meck:expect(gen_tcp, send, fun(_, _) -> ok end),

    meck:expect(siftbulk_ftp, read_reply, 
                fun(_, _) -> {ok, <<"331">>, "Test"} end),

    Result = siftbulk_ftp:do_login_step1(Socket, "TestKey", "123fd-4"),
    
    ?assert(meck:called(siftbulk_ftp, read_reply,
                        [Socket, 10000])),
    ?assertEqual(Result, {ok, "Socket"}).

%% do_login_step2

test_do_login_step2_multiline_failure() ->
    Socket = "Socket",
    meck:expect(siftbulk_ftp, do_login_step2, fun(_, Arg2) ->
                                                  meck:passthrough([Socket, 
                                                                    Arg2]) end),
    meck:expect(gen_tcp, send, fun(_, _) -> ok end),
    meck:expect(siftbulk_ftp, read_reply, 
                fun(_, _) -> {ok, <<"231">>, "Test"} end),

    Result = siftbulk_ftp:do_login_step2(Socket, "123fd-4"),
    
    ?assert(meck:called(gen_tcp, send, [Socket, <<"PASS 123fd-4\r\n">>])),
    ?assert(meck:called(siftbulk_ftp, read_reply,
                        [Socket, 10000])),
    ?assertEqual(Result, {error, {ok, <<"231">>, "Test"}}).

test_do_login_step2_multiline_success() ->
    Socket = "Socket",
    meck:expect(siftbulk_ftp, do_login_step2, fun(_, Arg2) ->
                                                  meck:passthrough([Socket, 
                                                                    Arg2]) end),
    meck:expect(gen_tcp, send, fun(_, _) -> ok end),
    meck:expect(siftbulk_ftp, read_reply, 
                fun(_, _) -> {ok, <<"230">>, "Test"} end),

    Result = siftbulk_ftp:do_login_step2(Socket, "123fd-4"),
    
    ?assert(meck:called(gen_tcp, send, [Socket, <<"PASS 123fd-4\r\n">>])),
    ?assert(meck:called(siftbulk_ftp, read_reply,
                        [Socket, 10000])),
    ?assertEqual(Result, {ok, Socket}).

%% connect

test_connect_failure() ->
    meck:expect(siftbulk_ftp, connect, fun(Arg1, Arg2, Arg3, Arg4) ->
                                            meck:passthrough([Arg1, Arg2, Arg3,
                                                              Arg4]) end),
    meck:expect(siftbulk_ftp, do_connect, fun(_, _) -> {error, "An Error"} end),

    Result = siftbulk_ftp:connect("localhost", 21, "TestKey", "123fd-4"),

    ?assert(meck:called(siftbulk_ftp, do_connect, ["localhost", 21])),
    ?assertEqual(Result, {error, "An Error"}).

test_connect_success() ->
    %% Socket does not need to be a socket object because do_login is mocked
    meck:expect(siftbulk_ftp, connect, fun(Arg1, Arg2, Arg3, Arg4) ->
                                            meck:passthrough([Arg1, Arg2, Arg3,
                                                              Arg4]) end),
    meck:expect(siftbulk_ftp, do_connect, fun(_, _) -> 
                                              {ok, "Socket", "A Message"} end),
    meck:expect(siftbulk_ftp, do_login_step1, fun(Socket, _, _) -> 
                                                  {ok, Socket} end),

    Result = siftbulk_ftp:connect("localhost", 21, "TestKey", "123fd-4"),

    ?assert(meck:called(siftbulk_ftp, do_connect, ["localhost", 21])),
    ?assertEqual(Result, {ok, "Socket"}).