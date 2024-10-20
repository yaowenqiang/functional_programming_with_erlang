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
                io:format("~n", Rest),
                [Succeeds|Remainder] = get_while(P, Rest),
                {[Ch|Succeeds], Remainder};
        false -> {[], [Ch|Rest]}
    end;
get_while(_P, []) ->
    {[], []}.
