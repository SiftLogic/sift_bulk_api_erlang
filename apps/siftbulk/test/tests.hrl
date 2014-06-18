-define(setup(ToTest), {setup, fun start/0, fun stop/1, [ToTest]}).

quiet_stop(App) ->
    error_logger:tty(false),
    Res = application:stop(App),
    error_logger:tty(true),
    Res.