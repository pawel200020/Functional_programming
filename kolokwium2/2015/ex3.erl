-module (ex3).
-compile (export_all).
process()->receive
            {Number,[C|ListOfProcess]} -> 
                io:format(" ~p \n", [Number]),
                C ! {Number+1,ListOfProcess++[C]}
            end,
            timer:sleep(1000),
            process().

startProcesses(1)->[spawn(ex3,process,[])];                                                
startProcesses(N)->[spawn(ex3,process,[])]++startProcesses(N-1).

start(N)->    List = startProcesses(N),
              lists:last(List) ! {1,List}.