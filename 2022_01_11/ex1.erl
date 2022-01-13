-module (ex1).
-export ([start/1, hello/2]).

hello(Msg, 1) -> io:format("tu proces: ~p ~n", [Msg]);
hello(Msg, N) -> io:format("tu proces:~p ~n", [Msg]),
                 hello(Msg, N-1).

start(1)-> spawn(ex1, hello, [1, 3]);
start(Count) -> spawn(ex1, hello, [Count, 3]),
                start(Count-1).