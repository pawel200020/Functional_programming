-module (ex3bis).
-export ([start/0,client/2, server/0]).

client([],PID) -> PID ! end_of_list;
client ([A|Rest],PID)-> PID ! {A,self()},
                        receive
                            X-> io:format("client recieved ~p \n",[X])
                        end,
                        client(Rest,PID).

server ()-> receive
                end_of_list ->  io:format("End of working.");

                {X,PID} ->      io:format("server recieved ~p \n",[X*X]),
                                PID ! X*X,
                                server()
                after 1000 ->   io:format("End of working.")
            end.
            

start() ->  Temp = spawn(ex3bis,server,[]),
            spawn(ex3bis,client,[[1,2,3,4,5],Temp]).