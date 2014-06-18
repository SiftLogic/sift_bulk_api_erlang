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

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    ok = application:start(siftbulk).
 
stop(_) ->
    ok = quiet_stop(siftbulk).

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
    ?assertEqual(<<"RECV test.csv\r\n">>, siftbulk_ftp:make_command("RECV", "test.csv")).