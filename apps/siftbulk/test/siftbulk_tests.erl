% erl -make
% erl -noshell -pa ebin -eval "eunit:test(siftbulk_tests, [verbose])" -s init stop
-module(siftbulk_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/siftbulk.hrl").

-define(DEFAULT_CONNECTION_INFO, [{host, <<"localhost">>},
                                  {port, 21},
                                  {poll_every, 300}]).

-define(DEFAULT_OPTS_INFO, #state{}#state.opts).
-define(setup(ToTest), {setup, fun start/0, fun stop/1, [ToTest]}).

quiet_stop(App) ->
    error_logger:tty(false),
    Res = application:stop(App),
    error_logger:tty(true),
    Res.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
%% These need to be tested together because they are the main getter/setter
set_opts_and_get_opts_test_() ->
    {"Verifies that set opts actually sets values and the can be retrieved",
     ?setup([fun test_get_opts_empty/0,
             fun test_get_opts_set_one/0,
             fun test_get_opts_set_one_list/0,
             fun test_get_opts_set_multiple/0])}.

init_test_() ->
    {"Verifies that init sets default and values when appropriate",
     ?setup([fun test_init_default_empty/0])}.

handle_call_test_() ->
    {"Verifies that it sends calls to all of the relevant functions",
     ?setup([fun test_call_set_opts/0,
             fun test_call_get_opts/0,
             fun test_call_stop/0,
             fun test_call_default/0])}.

handle_cast_test_() ->
    {"Verifies that it sends casts to all of the relevant functions",
     ?setup([fun test_cast_default/0])}.

handle_info_test_() ->
    {"Verifies that it sends info to all of the relevant functions",
     ?setup([fun test_info_default/0])}.

code_change_test_() ->
    {"Verifies that it updates code correctly",
     ?setup([fun test_code_change_default/0])}.

terminate_test_() ->
    {"Verifies that it terminates correctly",
     ?setup([fun test_terminate_default/0])}.

stop_test_() ->
    {"Verifies that it stops correctly",
     ?setup([fun test_stop_default/0])}.

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
test_get_opts_empty() ->
    ?assertEqual(?DEFAULT_OPTS_INFO, siftbulk:get_opts()).

test_get_opts_set_one() ->
    ShouldBe = lists:keyreplace(username, 1, ?DEFAULT_OPTS_INFO,
                                {username, <<"TestUser">>}),
    siftbulk:set_opts({username, <<"TestUser">>}),
    ?assertEqual(ShouldBe, siftbulk:get_opts()).

test_get_opts_set_one_list() ->
    ShouldBe = lists:keyreplace(username, 1, ?DEFAULT_OPTS_INFO,
                                {username, <<"TestUser">>}),
    siftbulk:set_opts([{username, <<"TestUser">>}]),
    ?assertEqual(ShouldBe, siftbulk:get_opts()).

test_get_opts_set_multiple() ->
    Temp = lists:keyreplace(username, 1, ?DEFAULT_OPTS_INFO,
                            {username, <<"TestUser">>}),
    Temp1 = lists:keyreplace(password, 1, Temp, {password, <<"test">>}),
    ShouldBe = lists:keyreplace(host, 1, Temp1, {host, <<"bacon">>}),

    siftbulk:set_opts([{username, <<"TestUser">>},
                       {password, <<"test">>},
                       {host, <<"bacon">>}]),
    ?assertEqual(ShouldBe, siftbulk:get_opts()).

test_init_default_empty() ->
    ?assertEqual({ok, #state{}}, 
                 siftbulk:init([])).

test_call_set_opts() ->
    Opts = [{username, <<"TestKey">>}],
    NewOpts = lists:keyreplace(username, 1, 
                               ?DEFAULT_OPTS_INFO, {username, <<"TestKey">>}),

    NewState = #state{opts = NewOpts},
    ?assertEqual({reply, ok, NewState}, 
                 siftbulk:handle_call({set_opts, Opts}, undefined, #state{})).

test_call_get_opts() ->
    ?assertEqual({reply, ?DEFAULT_OPTS_INFO, #state{}}, 
                 siftbulk:handle_call(get_opts, undefined, #state{})).

test_call_stop() ->
    ?assertEqual({stop, normal, ok, #state{}}, 
                 siftbulk:handle_call(stop, undefined, #state{})).

test_call_default() ->
    ?assertEqual({noreply, #state{}}, 
                 siftbulk:handle_call(undefined, undefined, #state{})).

test_cast_default() ->
    ?assertEqual({noreply, #state{}}, 
                 siftbulk:handle_cast(undefined, #state{})).

test_info_default() ->
    ?assertEqual({noreply, #state{}}, 
                 siftbulk:handle_info(undefined, #state{})).

test_code_change_default() ->
    ?assertEqual({ok, #state{}}, 
                 siftbulk:code_change(undefined, #state{}, undefined)).

test_terminate_default() ->
    ?assertEqual(ok, siftbulk:terminate(undefined, undefined)).

test_stop_default() ->
    ?assertEqual(ok, siftbulk:stop()).
