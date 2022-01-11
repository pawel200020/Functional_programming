-module (prog4).

-export ([posNeg/1]).

posNeg([])->{0,0};
posNeg([A|Rest])->  {P,N} = posNeg(Rest),
                     if A>0 ->
                        {P+1,N};
                    true ->
                        {P,N+1}
                    end.