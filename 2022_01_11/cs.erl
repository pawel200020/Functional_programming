-module (cs).
-export ([server/0,start/0,client/1]).

recvMsg() -> receive
                %% tutaj obsluga przychodzacych komunikatow, 
                %% czyli kod serwera.
             end,
             recvMsg().

server() -> io:format("Server started. Awaiting for messages ~n"),
            recvMsg().

client(SPID) -> SPID ! msg
                %% dzialanie klienta, to po prostu wyslanie atomu msg
                %% do serwera. Oczywiscie moze to byc bardziej skomplikowane...

start() -> Server_PID = spawn(cs,server,[]),
           spawn (cs, client, [Server_PID]).

