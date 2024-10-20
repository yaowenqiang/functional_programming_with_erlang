```erlang
print:print({add,{add,{num, 3}, {num,4}}, {mul, {num, 3}, {num, 4}}}).


-spec eval(expr()) -> integer().

```

## A stack virtual machine

+ The virtual machine has as stack
+ Which is manipulated by the machine instructions
+ For example the PUSH N instruction pushes the integer N onto the top of the stack

(2*(3*a)) compiles to the instruction sequence

PUSH 2 
PUSH 3
FETCH a
MUL 3
ADD 2


## doing it in Erlang

We need to show how it is all implemented in Erlang

+ How to model machine instructions
+ How to model the running of the machine
+ How to compile an expression into a sequence of instructions


### instructions, programs and stacks

```erlang
-type instr() :: {'push', integer()}
              |  {'fetch', atom()}
              |  {'add2'}
              |  {'mul2'}.

-type program() :: [instr()].
-type stack() :: [integer()].
```

### Compiling expressions: running programs

```erlang
-spec compile(expr()) -> program().
-spec run(program(), env()) -> integer.
-spec run(program(), env(), stack()) -> integer.
```
### running the stack machine

The stack machin run function is defined using pattern matching and tail recursion.

```erlang
run([{push, N} | Continue], Env, Stack) ->
    run(Continue, Env, [N | Stack]);
run([{fetch, A} | Continue], Env, Stack) ->
    run(Continue, Env, [lookup(A, Env) | Stack]);
run([{add2} | Continue], Env, [N1, N2, | Stack]) ->
    run(Continue, Env, [(N1 + N2) | Stack]);
run([{mul2} | Continue], Env, [N1, N2, | Stack]) ->
    run(Continue, Env, [(N1 * N2) | Stack]);
run([], Env, [N]) -> 
    N.
```

### Compilation

Numbers and (values of) variables go on the stack

To perform an add, evaluate the two sub-expression, putting each of the result on the stack, then add the values on top of the stack.

```erlang
-spec compile(expr()) -> program().

compile({num, N}) ->
    [{push, N}];
compile({var, N}) ->
    [{fetch, N}];
compile({add, E1, E2}) ->
    compile(E1) ++ compile(E2) ++ [{add2}];
compile({multi, E1, E2}) ->
    compile(E1) ++ compile(E2) ++ [{mul2}];
```

### Leason learned

Some of the lessions we can learn from this section are:

+ flexibility of data representation
+ type definitions and specifications
+ pattern matching
+ tail recursion
+ missing cases - let it fail

### Going further

+ Simplification: (0+(1*v)) simpifies to v
+ More operations: substraction, divirem, unary minus
+ Setting variables: let v=e1 in e2
+ Defining function for yourself: let f=(\x->e1) in e2
+ Adding other types: if b then e1 else e2
+ Changing the syntax: e.g, operator precedence - BODMAs


## how to run erlang observer

```bash

erl -sname observer -hidden -setcookie MyCookie -run observer

```


## debugger

https://www.erlang.org/doc/apps/debugger/debugger_chapter.html
```erlang
debugger:start().
```
