-module(siftbulk_ftp_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("tests.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
make_command_test_() ->
    {"Verifies that the passed in are converted to a correct format and it is
      in binary form",
     ?setup([fun test_make_command_empty/0,
             fun test_make_command_empty_argument/0,
             fun test_make_command_empty_command/0,
             fun test_make_command_filled_in/0])}.

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
    meck:new(siftbulk_ftp, [unstick]).
 
stop_mock(_Arg) ->
    meck:validate(siftbulk_ftp),
    meck:unload(siftbulk_ftp),
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
    meck:expect(siftbulk_ftp, do_login, fun(Socket, _, _) -> 
                                              {ok, Socket} end),

    Result = siftbulk_ftp:connect("localhost", 21, "TestKey", "123fd-4"),

    ?assert(meck:called(siftbulk_ftp, do_connect, ["localhost", 21])),
    ?assertEqual(Result, {ok, "Socket"}).