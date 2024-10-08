-module(concurrent).
-export(export_all).

% Exercise 1
ping() ->
  receive
    {req,Msg,Pid} when is_pid(Pid) ->
      ClientPid ! {ack,Msg},
      ping();
    Wrong -> io:format("bad message~n")
  end.
  

% Exercise 2
fib(0) -> 1;
fib(1) -> 1;
fib(N) when is_integer(N), N > 1 -> fib(N-2) + fib(N-1).

fibserver() ->
  receive
    {fib,N,Pid} when is_pid(Pid) ->
      spawn(fun() ->
              Result = fib(N),
              Pid ! {fib,N,is,Result}
            end),
      fibserver()
  end.
  
% Exercise 3
largestNum(V,P,Q) -> % To start with this function properly, run it with a process as "largestNum(0,0,0)"
  receive
    {put,N} when N > V ->
      largestNum(N,P+1,Q);
    {put,N} ->
      largestNum(V,P+1,Q);
    {queryy,Pid} when is_pid(Pid) ->
      Pid ! {largest,V},
      largestNum(V,P,Q+1);
    {statistics,Pid} when is_pid(Pid) ->
      Pid ! {P,Q},
      largestNum(V,P,Q)
  end.
  
% Exercise 4
n_connected_cells(N, Pid) ->
  Next = spawn(connect_cell(N,Pid)),
  Next.

connect_cell(0,Pid) ->
  receive
    X ->
      Pid ! X
      connect_cell(0,Pid)
  end.
connect_cell(N,Pid) when N > 0 ->
  Next = spawn(connect_cell(N-1,Pid)),
  sendNext(Next).

sendNext(Pid) ->
  receive
    X ->
      Pid ! X,
      sendNext(Pid)
  end.
  
% Exercise 5
-module(bank).
-export([create_bank/0, new_account/2, withdraw_money/3, deposit_money/3, transfer/4]).

create_bank() ->
   spawn(fun () -> my_bank(#{}) end).
my_bank(Accounts) ->
  receive
    {new_account,AccountNumber,From} when maps:is_key(AccountNumber, Accounts) ->
      From ! io:format("Existing account"),
      my_bank(Accounts);
    {new_account,AccountNumber,From} when not maps:is_key(AccountNumber, Accounts) ->
      maps:put(AccountNumber, 0, Accounts),
      From ! io:format("Succesfully created account ~p~n", [AccountNumber]),
      my_bank(Accounts);
    {withdraw, AccountNumber, Quantity, From} when Quantity =< maps:get(AccountNumber, Accounts) ->
      Result = maps:get(AccountNumber, Accounts) - Quantity,
      maps:put(AccountNumber, Result, Accounts),
      From ! io:format("Withdrawed ~p and the balance is now ~p~n", [Quantity,Result]),
      my_bank(Accounts);
    {withdraw, AccountNumber, Quantity, From} when Quantity > maps:get(AccountNumber, Accounts) ->
      From ! io:format("Withdraw failed. Balance is ~p~n", [maps:get(AccountNumber, Accounts)]),
      my_bank(Accounts);
    {deposit, AccountNumber, Quantity, From} ->
      Result = maps:get(AccountNumber, Accounts) + Quantity,
      maps:put(AccountNumber, Result, Accounts),
      From ! io:format("Succesfully deposited ~p in account ~p. Balance is ~p~n", [Quantity, AccountNumber, Result]),
      my_bank(Accounts);
    {transfer, FromA, ToA, Q, From} when Q =< maps:get(FromA, Accounts) ->
      Result = maps:get(FromA, Accounts) - Quantity,
      maps:put(FromA, Result, Accounts),
      Result2 = maps:get(ToA, Accounts) + Quantity,
      maps:put(ToA, Result2, Accounts),
      From ! io:format("Succesfully transferred ~p from account to account ~p~n", [Quantity, FromA, ToA]),
    {balance, AccountNumber} ->
      From ! io:format("Balance is ~p~n", [maps:get(AccountNumber, Accounts)]),
      my_bank(Accounts);
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

balance(Bank,Account)                           ->
  Bank ! {balance, Account, self()},
  receive Reply -> Reply end.
