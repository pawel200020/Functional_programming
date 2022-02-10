-module (ex3).
-compile (export_all).
process()->receive
            {[A|ListOfNumbers],[C|ListOfProcess]} -> 
                io:format(" ~p \n", [([A]++ListOfNumbers)]),
                C ! {([A+1]++[A]++ListOfNumbers),ListOfProcess++[C]}                                
            end,
            timer:sleep(1000),
            process().

startProcesses(1)->[spawn(ex3,process,[])];                                                
startProcesses(N)->[spawn(ex3,process,[])]++startProcesses(N-1).

start(N)->    List = startProcesses(N),
                io:format(" ~p \n", [List]),
                lists:last(List) ! {[1],List}.