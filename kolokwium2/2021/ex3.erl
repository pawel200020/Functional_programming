-module (ex3).
-compile (export_all).
process()->receive
            {[A,B|ListOfNumbers],[C|ListOfProcess]} ->  Temp = A + B,
                                                        List = [A]++[B]++ListOfNumbers,
                                                        io:format(" ~p \n", [List]),
                                                        List2 = [Temp]++List,                                                        
                                                       if
                                                            length(List2)>4 ->
                                                                C ! {lists:sublist(List2,2),ListOfProcess++[C]};
                                                            true ->
                                                                C ! {List2,ListOfProcess++[C]}
                                                        end
                                                    
            end,
            timer:sleep(1000),
            process().

startProcesses(1)->[spawn(ex3,process,[])];                                                
startProcesses(N)->[spawn(ex3,process,[])]++startProcesses(N-1).

start(N,K)->    List = startProcesses(N),
                io:format(" ~p \n", [List]),
                lists:last(List) ! {[K,1],List}.