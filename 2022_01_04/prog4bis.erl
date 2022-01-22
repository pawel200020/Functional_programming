-module (prog4bis).
-export ([posNeg/1]).

posNeg([])->{0,0};
posNeg([A|Rest])->  {N,K} = posNeg (Rest),
                    if 
                        A > 0->
                            {N+1, K};
                        true ->
                            {N,K+1}
                    end.

