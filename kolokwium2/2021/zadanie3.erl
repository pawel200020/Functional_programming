-module (zadanie3).
-compile (export_all).
process()->receive
            {Number,[{Id,C}|ListOfProcess]} ->  
                                            {IdMy, _} = lists:last(ListOfProcess),     
                                            io:format(" (~p,~p) \n", [IdMy,Number]),                                                      
                                            if
                                                is_list(Number) ->
                                                    Var = (length(ListOfProcess)+1)*(length(ListOfProcess)+1),
                                                    Var2 = lists:sum(Number),
                                                    if 
                                                        Var > Var2->
                                                            C ! {[IdMy*lists:last(lists:reverse(Number))]++Number ,ListOfProcess++[{Id,C}]};
                                                        true ->
                                                            C ! {IdMy*lists:last(lists:reverse(Number)) ,ListOfProcess++[{Id,C}]}
                                                    end;
                                                true ->
                                                    C ! {[IdMy,Number],ListOfProcess++[{Id,C}]}
                                            end
                                                    
            end,
            timer:sleep(1000),
            process().

sumList([])->0;
sumList ([A|Rest]) -> A+sumList(Rest).

startProcesses(2)->[{2,spawn(zadanie3,process,[])}];                                                
startProcesses(N)->[{N,spawn(zadanie3,process,[])}]++startProcesses(N-1).

start(N,X)->    List = startProcesses(N),
                List2 = lists:reverse(List)++[{1,spawn(zadanie3,process,[])}],
                {_,PID}=lists:last(List2),
                PID ! {X,List2}.