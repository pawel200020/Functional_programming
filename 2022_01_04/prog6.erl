-module (prog6).

-export ([rownanie/3]).

rownanie(A,B,C)-> if B*B-4*A*C>0 ->
                        {-b+math:sqrt(B*B-4*A*C)/2*a,b+math:sqrt(B*B-4*A*C)/2*a};
                    B*B-4*A*C<0->
                        brakRoazwiazn;
                    true->
                        (-B) / 2 * A
                    end.