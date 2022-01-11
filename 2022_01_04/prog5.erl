-module (prog5).

-export ([split/1]).

split([])->{[],[]};
split([A|Rest]) ->  {I,R}=split(Rest),
                    if is_integer(A) -> {A++I,R};
                    true->{I,[A]++R}
                    end.