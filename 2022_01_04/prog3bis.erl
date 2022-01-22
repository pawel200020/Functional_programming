-module (prog3bis).
-export ([even_pos/1,dlugosc/1]).

even_pos ([]) -> [];
even_pos ([A]) ->[A];
even_pos ([A,_|Rest])-> [A]++(even_pos (Rest)). 

dlugosc ([])->0;
dlugosc ([_|Rest]) -> 1 + dlugosc (Rest).