-module(high_order_func).
-export([all2/2, foldl2/3, incrementBy1/1, map2/2, tree/2]).

all2(_,[]) -> false;
all2(F,[X]) -> F(X);
all2(F, [X | Rest]) -> F(X) and all2(F,Rest).

foldl2(F, acc, []) -> acc;
foldl2(F, acc, [X | Rest]) -> foldl2(F, F(acc,X), Rest).

map2(F, []) -> [];
map2(F, [X | Rest]) -> [F(X) | map2(F, Rest)].

incrementBy1(X) -> X+1.

tree(F, void) -> void;
tree(F, {node, X, Y, Z}) -> {node, F(X), tree(F, Y), tree(F, Z)}.
