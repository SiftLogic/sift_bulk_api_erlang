% Stores connection information visible to users of the ftp server.
-record(state, {opts = [{username, undefined},
                        {password, undefined},
                        {host, "localhost"},
                        {port, 21},
                        {poll_every, 300}],
                connection = undefined,
                data = undefined}).
