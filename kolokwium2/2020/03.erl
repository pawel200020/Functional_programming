-module (ex3).
-compile (export_all).

recvMsg() -> receive
                {X} ->  io:format("~p ~n",[X*X]),
                        recvMsg();
                end_of_list -> exit(normal)
             end.

server() -> io:format("Server started. Awaiting for messages ~n"),
            recvMsg().

client(SPID, []) -> SPID ! end_of_list;
client(SPID, [X|Rest]) ->   SPID ! {X},
                            client(SPID, Rest).


start() -> Server_PID = spawn(ex3,server,[]),
           spawn (ex3, client, [Server_PID,[1,2,3,4,5,6]]).
