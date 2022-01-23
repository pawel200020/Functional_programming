-module (ex1bis).
-export ([start/1,write/1]).

write (1)-> io:format("Tu proces ~p \n",[self()]);
write (K)-> io:format("Tu proces ~p \n",[self()]),
            write(K-1).

start(1) -> spawn(ex1bis,write,[3]);
start(K) -> spawn(ex1bis,write,[3]),
            start(K-1).
