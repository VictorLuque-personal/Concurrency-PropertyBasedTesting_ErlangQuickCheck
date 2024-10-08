-module(seq_ex).
-export([fib/1, fib2/1, sum/1, member/2, insert/2, sort/1, fibindex/1, keyFind/2, merge/2]).
fib(0) -> 1;
fib(1) -> 1;
fib(N) when is_integer(N), N > 1 -> fib(N-2) + fib(N-1).

fib2(N) ->
  if
    N == 0                  -> 1;
    N == 1                  -> 1;
    N > 1, is_integer(N) -> fib2(N-2) + fib2(N-1)
  end.
  
sum([])                            -> 0;
sum([N | Rest]) when is_integer(N) -> N + sum(Rest);
sum([_ | Rest])                    -> sum(Rest).

member(_, [])                   -> false;
member(X, [Y | Ys]) when X == Y -> true;
member(X, [Y | Ys])             -> member(X, Ys).

insert(X, []) when is_integer(X)                -> [X];
insert(X, [Y | Ys]) when is_integer(X), X > Y   -> [Y | insert(X, Ys)];
insert(X, [Y | Ys]) when is_integer(X), X =< Y  -> [X | [Y | Ys]].

sort([]) -> [];
sort([X | Xs]) -> lists:append(lists:append(sort(lists:filter(fun(Y) -> Y =< X end, Xs)), [X]), sort(lists:filter(fun(Y) -> Y > X end, Xs))).

fibindex(0) -> {0,1};
fibindex(1) -> {1,1};
fibindex(N) when is_integer(N), N > 1 -> {N,fib(N)}.

keyFind(Key, []) -> false;
keyFind(Key, [{Key,Val} | Rest]) -> {Key,Val};
keyFind(Key, [{Key2,_} | Rest]) -> keyFind(Key, Rest).

merge([], L)                                -> L;
merge(L, [])                                -> L;
merge([X | RestX], [Y | RestY]) when X =< Y -> [X | merge(RestX, [Y | RestY])];
merge([X | RestX], [Y | RestY]) when X > Y  -> [Y | merge([X | RestX], RestY)].
