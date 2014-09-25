-module(siftbulk_example).

-export([run/0]).

run() ->
    Username = "",
    Password = "",
    Host = "",
    Port = 21,

    {ok, Pid} = siftbulk:connect(Username, Password, Host, Port),
    ok = siftbulk:cd(Pid, "Path"),
    case siftbulk:upload(Pid, "Filename") of
        {ok, Message} ->
            Message;
        {error, Reason} ->
            Reason
    end,

    siftbulk:disconnect(Pid).

