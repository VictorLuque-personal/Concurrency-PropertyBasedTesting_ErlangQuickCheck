-module(bank).
-compile(export_all).

create_bank() ->
   spawn(fun () -> my_bank(#{}) end).
my_bank(Accounts) ->
  receive
    {new_account,AccountNumber,From} ->
      Aux = maps:is_key(AccountNumber, Accounts),
      if
        Aux == true ->
          From ! ko,
          my_bank(Accounts);
        Aux == false ->
          NewMap = maps:put(AccountNumber, 0, Accounts),
          From ! ok,
          my_bank(NewMap)
      end;
    {withdraw, AccountNumber, Quantity, From} ->
      Aux = (Quantity =< maps:get(AccountNumber, Accounts)),
      if
        Aux == true ->
          Result = maps:get(AccountNumber, Accounts) - Quantity,
          NewMap = maps:put(AccountNumber, Result, Accounts),
          From ! ok,
          my_bank(NewMap);
        Aux == false ->
          From ! ko,
          my_bank(Accounts)
      end;
    {deposit, AccountNumber, Quantity, From} ->
      Result = maps:get(AccountNumber, Accounts) + Quantity,
      NewMap = maps:put(AccountNumber, Result, Accounts),
      From ! NewMap,
      my_bank(NewMap);
    {transfer, FromA, ToA, Q, From} ->
      Aux = (Q =< maps:get(FromA, Accounts)),
      if
        Aux == true ->
          Result = maps:get(FromA, Accounts) - Q,
          NewMap2 = maps:put(FromA, Result, Accounts),
          Result2 = maps:get(ToA, NewMap2) + Q,
          NewMap = maps:put(ToA, Result2, NewMap2),
          From ! ok,
          my_bank(NewMap);
        Aux == false ->
          From ! ko,
          my_bank(Accounts)
      end;
    {balance, AccountNumber, From} ->
      Aux = maps:is_key(AccountNumber, Accounts),
      if
        Aux == true -> From ! maps:get(AccountNumber, Accounts);
        Aux == false -> From ! ko
      end,
      my_bank(Accounts)
  end.

new_account(Bank,AccountNumber) ->
  Bank!{new_account,AccountNumber,self()},
  receive Reply -> Reply end.

withdraw_money(Bank,AccountNumber,Quantity) ->
  Bank ! {withdraw, AccountNumber, Quantity, self()},
  receive Reply -> Reply end.

deposit_money(Bank,AccountNumber,Quantity) ->
  Bank ! {deposit, AccountNumber, Quantity, self()},
  receive Reply -> Reply end.

transfer(Bank,FromAccount, ToAccount, Quantity) ->
  Bank ! {transfer, FromAccount, ToAccount, Quantity, self()},
  receive Reply -> Reply end.

balance(Bank,Account) ->
  Bank ! {balance, Account, self()},
  receive Reply -> Reply end.
