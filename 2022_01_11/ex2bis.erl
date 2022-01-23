-module (ex2bis).
-export ([start/0,procA/1, procB/0]).

procA (PID) ->  io:format("Proces A stared with PID: ~p \n",[self()]),
                io:format("Proces A sends atom czesc to B"),
                PID ! czesc.

procB () -> io:format("Proces B stared with PID: ~p \n",[self()]),
            receive
                X -> io:format("Proces B recieved ~p \n",[X])
            end.

start() ->  Temp = spawn(ex2bis,procB,[]),
            spawn(ex2bis,procA,[Temp]).
