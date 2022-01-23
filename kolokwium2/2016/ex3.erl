-module (ex3).
-compile (export_all).

first ()-> receive
                {X,D}->
                    io:format("~p ~n", [X]),
                    if
                        abs(X)>100 ->
                            second_a ! {X-D,-D};
                        true ->
                            second_a ! {X+D,D}
                    end
            end,
            timer:sleep(100),
            first().

second ()-> receive
                {X,D}->
                    io:format("~p ~n", [X]),
                    if
                        abs(X)>100 ->
                            first_a ! {X-D,-D};
                        true ->
                            first_a ! {X+D,D}
                    end
            end,
            timer:sleep(100),
            second().
            

start(X,D) ->  N = spawn(ex3,first,[]),
            S = spawn(ex3,second,[]),
            register(first_a,N),
            register(second_a,S),
            first_a ! {X,D}.