-module(quviq_test).
-compile(export_all).

-include_lib ("eqc/include/eqc.hrl").

% TESTING WITH QUICKCHECK

test_fib2_correctness() -> 
  ?FORALL(X, nat(), fib2(X) == fib(X)).
  
test_sum() ->
  ?FORALL(X, list(oneof([int(), char()])), case lists:sum(X) of
                                                      {"EXIT", _ }  -> true;
                                                      _             -> lists:sum(X) == sum(X)
                                                   end).

sized_list_int(0, _) -> [];
sized_list_int(N, G) -> [ G | sized_list_int(N-1, G) ].
                                                   
test_sort() ->
  ?FORALL(L, ?SIZED(Size, sized_list_int(Size, oneof([choose(-10000, 10000), choose(-10, 10)]))), lists:sort(L) == sort(L)).
  
tree_gen(0) -> void;
tree_gen(N) ->
  case N rem 2 of
    0 -> oneof([{node, int(), tree_gen((N-1) div 2), tree_gen(((N-1) div 2) + 1)}, {node, int(), tree_gen(((N-1) div 2) + 1), tree_gen((N-1) div 2)}]);
    1 -> {node, int(), tree_gen(N div 2), tree_gen(N div 2)}
  end.
  
test_trees() ->
  ?FORALL(T, ?SIZED(Size, tree_gen(Size)), tree(fun(X) -> X+1 end, T) == tree_plus_one(T)).


% SOME FIRST HOMEWORK CODE with some created new one for trees

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

tree(F, void) -> void;
tree(F, {node, X, Y, Z}) -> {node, F(X), tree(F, Y), tree(F, Z)}.

tree_plus_one(void) -> void;
tree_plus_one({node, X, Y, Z}) -> {node, X+1, tree_plus_one(Y), tree_plus_one(Z)}.
