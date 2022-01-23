-module (ex3).
-compile (export_all).
process()->receive
            {Number,[{Id,C}|ListOfProcess]} ->  
                                            {IdMy, _} = lists:last(ListOfProcess),     
                                            io:format(" (~p,~p) \n", [IdMy,Number]),                                                      
                                            if
                                                IdMy rem 2 == 0 ->
                                                    C ! {Number*2,ListOfProcess++[{Id,C}]};
                                                true ->
                                                    C ! {Number-1,ListOfProcess++[{Id,C}]}
                                            end
                                                    
            end,
            timer:sleep(1000),
            process().

startProcesses(2)->[{2,spawn(ex3,process,[])}];                                                
startProcesses(N)->[{N,spawn(ex3,process,[])}]++startProcesses(N-1).

start(N,X)->    List = startProcesses(N),
                List2 = lists:reverse(List)++[{1,spawn(ex3,process,[])}],
                {_,PID}=lists:last(List2),
                PID ! {X,List2}.