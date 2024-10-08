-module(bank_eqc).
-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

initial_state() -> { undefined, #{} }.
bank({Bank,_}) -> Bank.
set_bank(Bank,{_,Accounts}) -> {Bank,Accounts}.
accounts({_,Accounts}) -> Accounts.
set_accounts(Accounts,{Bank,_}) -> {Bank,Accounts}.

create_bank_pre(State) ->  bank(State) == undefined.
create_bank_args(_) -> [].
create_bank() -> bank:create_bank().
create_bank_next(State,Result,_) -> set_bank(Result,State).
create_bank_post(_,_,_) -> true.

new_account_pre(State) -> bank(State) =/= undefined.
new_account_args(State) -> [bank(State), nat()].
new_account(Bank,Key) -> bank:new_account(Bank,Key).
new_account_next(State,_,[_,Key]) -> 
  case maps:is_key(Key, accounts(State)) of
    false -> set_accounts(maps:put(Key,0,accounts(State)), State);
    true -> State
  end.
new_account_post(_, _, _) -> true.

balance_pre(State) -> maps:size(accounts(State)) > 0.
balance_args(State) -> [bank(State), frequency([{0.8, oneof(maps:keys(accounts(State)))}, {0.2, nat()}])].
balance(Bank,Key) -> bank:balance(Bank,Key).
balance_next(State,_,_) -> State.
balance_post(State,[_,Account],Result) ->
  case maps:is_key(Account, accounts(State)) of
    true -> Result == maps:get(Account, accounts(State));
    false -> Result == ko
  end.

deposit_money_pre(State) -> maps:size(accounts(State)) > 0.
deposit_money_args(State) -> [bank(State), oneof(maps:keys(accounts(State))), nat()].
deposit_money(Bank,Key,Money) -> bank:deposit_money(Bank,Key,Money).
deposit_money_next(State,_,[_,Key,Money]) -> set_accounts(maps:put(Key,maps:get(Key,accounts(State))+Money,accounts(State)),State).
deposit_money_post(State,[_,Key,Money],Result) -> maps:put(Key,maps:get(Key,accounts(State))+Money,accounts(State)) == Result.

withdraw_money_pre(State) -> maps:size(accounts(State)) > 0.
withdraw_money_args(State) -> [bank(State), oneof(maps:keys(accounts(State))), nat()].
withdraw_money(Bank,Key,Money) -> bank:withdraw_money(Bank,Key,Money).
withdraw_money_next(State,_,[_,Key,Money]) ->
  case Money =< maps:get(Key,accounts(State)) of
    true -> set_accounts(maps:put(Key,maps:get(Key,accounts(State))-Money,accounts(State)),State);
    false -> State
  end.
withdraw_money_post(State,[_,Key,Money],Result) ->
  case Result of
    ok -> Money =< maps:get(Key,accounts(State));
    ko -> Money > maps:get(Key,accounts(State))
  end.

%transfer_pre(State) -> maps:size(accounts(State)) > 1.
%transfer_args(State) -> [bank(State), oneof(maps:keys(accounts(State))), oneof(maps:keys(accounts(State))), nat()].
%transfer([Bank,Acc1,Acc2,Money]) -> bank:transfer(Bank,Acc1,Acc2,Money).
%transfer_next(State,_,[_,FromA,ToA,Money]) ->
%  case Money =< maps:get(FromA, accounts(State)) of
%    true ->
%      Result = maps:get(FromA, accounts(State)) - Money,
%      NewMap2 = maps:put(FromA, Result, accounts(State)),
%      Result2 = maps:get(ToA, NewMap2) + Money,
%      NewMap = maps:put(ToA, Result2, NewMap2),
%      set_accounts(NewMap,State);
%    false -> State
%  end.
%transfer_post(State,[_,Acc1,_,Money],Result) ->
%  case Result of
%    ok -> Money =< maps:get(Acc1,accounts(State));
%    ko -> Money > maps:get(Acc1,accounts(State))
%  end.

% To check the implementation; use eqc:quickcheck(bank_eqc:prop_bank()).
prop_bank() ->
  ?FORALL(Cmds,commands(bank_eqc),
  begin
    {H, S, Result} = run_commands(?MODULE,Cmds),
    pretty_commands(bank_eqc, Cmds, {H, S, Result}, Result == ok)
  end).	    

% To sample the test cases
sample() ->
  eqc_gen:sample(commands(bank_eqc)).
