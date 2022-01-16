-module (prog7).
-export ([subsets/2]).

append (A, L) -> [A] ++ L.

subsets ([], _) -> [];
subsets (_, 0) -> [[]];
subsets ([A|Rest], 1) -> [[A]] ++ subsets (Rest, 1);
subsets ([A|Rest], K) ->
  lists:map (fun (L) -> append (A, L) end, subsets (Rest, (K-1))) ++ subsets (Rest, K).