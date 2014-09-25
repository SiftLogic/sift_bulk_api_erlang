-module(siftbulk_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/siftbulk.hrl").
-include("tests.hrl").

-define(DEFAULT_OPTS_INFO, #state{}#state.opts).

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

connect_test_() ->
    {"Verifies that init sets default and values when appropriate",
     ?setup_mock([fun test_connect_failure/0,
                  fun test_connect_success/0])}.

handle_call_test_() ->
    {"Verifies that it sends calls to all of the relevant functions",
     ?setup_mock([fun test_handle_call_set_opts/0,
                  fun test_handle_call_get_opts/0,
                  fun test_handle_call_connect/0,
                  fun test_handle_call_stop/0,
                  fun test_handle_call_default/0])}.

handle_cast_test_() ->
    {"Verifies that it sends casts to all of the relevant functions",
     ?setup([fun test_handle_cast_default/0])}.

handle_info_test_() ->
    {"Verifies that it sends info to all of the relevant functions",
     ?setup([fun test_handle_info_default/0])}.

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

%% get_opts

test_get_opts_empty() ->
    ?assertEqual(?DEFAULT_OPTS_INFO, siftbulk:get_opts()).

test_get_opts_set_one() ->
    ShouldBe = lists:keyreplace(username, 1, ?DEFAULT_OPTS_INFO,
                                {username, "TestUser"}),
    siftbulk:set_opts({username, "TestUser"}),
    ?assertEqual(ShouldBe, siftbulk:get_opts()).

test_get_opts_set_one_list() ->
    ShouldBe = lists:keyreplace(username, 1, ?DEFAULT_OPTS_INFO,
                                {username, "TestUser"}),
    siftbulk:set_opts([{username, "TestUser"}]),
    ?assertEqual(ShouldBe, siftbulk:get_opts()).

test_get_opts_set_multiple() ->
    Temp = lists:keyreplace(username, 1, ?DEFAULT_OPTS_INFO,
                            {username, "TestUser"}),
    Temp1 = lists:keyreplace(password, 1, Temp, {password, "test"}),
    ShouldBe = lists:keyreplace(host, 1, Temp1, {host, "bacon"}),

    siftbulk:set_opts([{username, "TestUser"},
                       {password, "test"},
                       {host, "bacon"}]),
    ?assertEqual(ShouldBe, siftbulk:get_opts()).

%% init

test_init_default_empty() ->
    ?assertEqual({ok, #state{}}, 
                 siftbulk:init([])).

%% connect

test_connect_failure() ->
    Error = {error, "Error"},
    meck:expect(siftbulk_ftp, connect, fun(_, _, _, _) -> Error end),

    ?assertEqual(Error, siftbulk:connect()).

test_connect_success() ->
    Message = {ok, "Socket", "PassiveSocket"},
    meck:expect(siftbulk_ftp, connect, fun(_, _, _, _) -> Message end),

    ?assertEqual({ok, "Socket"}, siftbulk:connect()).

%% handle_call (Some of the branch logic is tested by public functions)

test_handle_call_set_opts() ->
    Opts = [{username, "TestKey"}],
    NewOpts = lists:keyreplace(username, 1, 
                               ?DEFAULT_OPTS_INFO, {username, "TestKey"}),

    NewState = #state{opts = NewOpts},
    ?assertEqual({reply, ok, NewState}, 
                 siftbulk:handle_call({set_opts, Opts}, undefined, #state{})).

test_handle_call_get_opts() ->
    ?assertEqual({reply, ?DEFAULT_OPTS_INFO, #state{}}, 
                 siftbulk:handle_call(get_opts, undefined, #state{})).

test_handle_call_connect() ->
    %% Simulating socket as a string instead of having to open a connection
    Socket = "Socket",
    PassiveSocket = "PassiveSocket",

    meck:expect(siftbulk_ftp, connect, fun(_, _, _, _) -> {ok, Socket, PassiveSocket} end),

    ?assertEqual({reply, {ok, Socket}, #state{connection = Socket, data = PassiveSocket}}, 
                 siftbulk:handle_call(connect, undefined, #state{})).

test_handle_call_stop() ->
    ?assertEqual({stop, normal, ok, #state{}}, 
                 siftbulk:handle_call(stop, undefined, #state{})).

test_handle_call_default() ->
    ?assertEqual({noreply, #state{}}, 
                 siftbulk:handle_call(undefined, undefined, #state{})).

%% handle_cast

test_handle_cast_default() ->
    ?assertEqual({noreply, #state{}}, 
                 siftbulk:handle_cast(undefined, #state{})).

%% handle_info

test_handle_info_default() ->
    ?assertEqual({noreply, #state{}}, 
                 siftbulk:handle_info(undefined, #state{})).

%% code_change

test_code_change_default() ->
    ?assertEqual({ok, #state{}}, 
                 siftbulk:code_change(undefined, #state{}, undefined)).

%% terminate

test_terminate_default() ->
    ?assertEqual(ok, siftbulk:terminate(undefined, undefined)).

%% stop

test_stop_default() ->
    ?assertEqual(ok, siftbulk:stop()).
