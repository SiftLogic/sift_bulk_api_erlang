-record(state, {opts = [{username, undefined},
                        {password, undefined},
                        {host, "localhost"},
                        {port, 21},
                        {poll_every, 300}],
                connection = undefined}).
