-module (prog6bis).
-export ([rownanie/3]).

rownanie (A,B,C)-> 
                    Delta = B*B-4*A*C,
                    if 
                        Delta > 0 ->
                            {(-B-math:sqrt(Delta))/2*A,(-B+math:sqrt(Delta))/2*A};
                        Delta == 0 ->
                            (-B)/2*A;
                        true->
                            brakRozwiazan
                    end.
