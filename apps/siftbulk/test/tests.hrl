-define(setup(Test), {setup, fun start/0, fun stop/1, [Test]}).
-define(setup_mock(Test), {setup, fun start_mock/0, fun stop_mock/1, [Test]}).

quiet_stop(App) ->
    error_logger:tty(false),
    Res = application:stop(App),
    error_logger:tty(true),
    Res.