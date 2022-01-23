-module (ex4).
-export ([start/0,client/1, server/0]).

client([]) -> atom ! koniec;
client ([A|Rest])-> atom ! A,
                        client(Rest).

server ()-> receive
                koniec ->               io:format("End of working.");

                X when is_list(X) ->    io:format("received list ~p \n", [X]),
                                        server();
                X when is_number(X) ->  io:format("received number ~p \n", [X]),
                                        server();
                X when is_atom(X) ->  io:format("received atom ~p \n", [X]),
                                        server()
            end.
            

start() ->  N = spawn(ex4,server,[]),
            register(atom,N),  
            spawn(ex4,client,[[1,2,3,at,[1,2,4]]]).