-module (prog3).

-export ([even_pos/1,dlugosc/1]).

dlugosc([])->0;
dlugosc([_|Rest])->1+dlugosc(Rest).

even_pos([]) ->[];
even_pos([A]) -> [A];
even_pos([A,_|Rest]) -> [A]++(even_pos(Rest)).
