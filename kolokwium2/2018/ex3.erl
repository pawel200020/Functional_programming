-module (ex3).
-compile (export_all).
process()->receive
            {Number,[{Id,C}|ListOfProcess]} ->  
                                            {IdMy, _} = lists:last(ListOfProcess),     
                                            io:format(" ~p\n", [Number]),                                                      
                                            if
                                                IdMy == 1 ->
                                                    C ! {Number+1,ListOfProcess++[{Id,C}]};
                                                IdMy == 2 ->
                                                    C ! {Number*2,ListOfProcess++[{Id,C}]};
                                                IdMy == 3 ->
                                                    C ! {Number-3,ListOfProcess++[{Id,C}]}
                                            end
                                                    
            end,
            timer:sleep(1000),
            process().

startProcesses(2)->[{2,spawn(ex3,process,[])}];                                                
startProcesses(N)->[{N,spawn(ex3,process,[])}]++startProcesses(N-1).

start(X)->      List = startProcesses(3),
                List2 = lists:reverse(List)++[{1,spawn(ex3,process,[])}],
                {_,PID}=lists:last(List2),
                PID ! {X,List2}.