-spec env :: [{atom(), integer()}].
-spec eval(env().expr()) -> integer().
eval(_Env,{num, N}) ->
  N;
eval(Env,{var, A}) ->
  lookup(A, Env);
eval(Env,{add, E1, E2}) ->
  eval(Env,E1) + eval(Env,E2);
eval({mul, E1, E2}) ->
  eval(Env,E1) * eval(Env,E2);
