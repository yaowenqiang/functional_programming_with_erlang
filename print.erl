-module(print).
-export([
         print/1
        ]).

print({num, N}) ->
    integer_to_list(N);
print({var, A}) ->
    atom_to_list(A);
print({add, E1, E2}) ->
  "(" ++ print(E1) ++ "+" ++ print(E2) ++  ")";
print({mul, E1, E2}) ->
  "(" ++ print(E1) ++ "*" ++ print(E2) ++  ")".

% print:print({add, {num, 2}, {mul, {num, 3}, {num, 4}}}).
   
