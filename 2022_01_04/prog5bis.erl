-module (prog5bis).
-export ([split/1]).

split ([])->{[],[]};
split ([A|Rest])->{X,Y} = split(Rest),
                    if
                        is_integer(A)->
                            {[A]++X,Y};
                        true ->
                            {X,[A]++Y}
                    end.