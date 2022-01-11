-module (prog2).

-export ([lucas/1]).

lucas(0) -> 2;
lucas(1) -> 1;
lucas(N) -> lucas(N-1)+lucas(N-2).