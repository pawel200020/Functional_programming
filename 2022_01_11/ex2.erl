-module (ex2).
-export ([start/0,procA/1,procB/0]).

% recvMsg() -> receive
%                 %% tutaj obsluga przychodzacych komunikatow, 
%                 %% czyli kod serwera.
%              end,
%              recvMsg().

% server() -> io:format("Server started. Awaiting for messages ~n"),
%             recvMsg().

% client(SPID) -> SPID ! msg
%                 %% dzialanie klienta, to po prostu wyslanie atomu msg
%                 %% do serwera. Oczywiscie moze to byc bardziej skomplikowane...
procA(SPID)->   io:format("Process ~p started", [self()]),
                SPID ! "czesc".
procB()->  io:format("Process ~p started", [self()]).
start() -> spawn(ex2,procA,[]).
