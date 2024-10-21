-module(parse).
-export([is_alpha/1, parse/1, get_while/2]).
-spec is_alpha(integer()) -> boolean().
-spec get_while(fun((T) -> boolean()), [T]) -> {[T],[T]}.

is_alpha(Ch) -> $a =< Ch andalso Ch =< $z.
parse([Ch|Rest]) when $a =< Ch andalso Ch =< $z ->
    {Succeeds, Remainder} = get_while(fun is_alpha/1, Rest),
    {{var, list_to_atom([Ch|Succeeds])}, Remainder}.

get_while(P, [Ch|Rest]) ->
    case P(Ch) of
        true -> 
                % io:format("~n", Rest),
                {Succeeds,Remainder} = get_while(P, Rest),
                {[Ch|Succeeds], Remainder};
        false -> {[], [Ch|Rest]}
    end;
get_while(_P, []) ->
    {[], []}.


zeroA({add, E, {num, 0}}) ->
    E;
zeroA({add, {num, 0}, E}) ->
    E;
zeroA(E) ->
    E.

mulO({mul, E, {num, 1}}) ->
    E;
mulO({mul, {num, 1}, E}) ->
    E;
mulO(E) ->
    E.
mulZ({mul, E, {num, 0}}) ->
    {num, 0};
mulZ({mul, {num, 0},_ }) ->
    {num, 0};
mulZ(E) ->
    E.

compose([]) ->
    fun(E) -> E end;
compose([Rule|Rules]) ->
    fun(E) -> (compose(Rules))(Rule(E)) end.

rules() ->
    [fun zeroA/1, fun mulO/1, fun mulZ/1].

simp(F, {add, E1, E2}) ->
    F({add, simp(F, E1), simp(F, E2)});
simp(F, {mul, E1, E2}) ->
    F({mul, simp(F, E1), simp(F, E2)});
simp(_, E) -> E .

simplify(E) ->
    simp(compose(rules()), E).

% expr:print(expr:simplify(expr:expr2()).