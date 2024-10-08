-module(fact).

-compile(export_all).

calc_fact(N) ->
  fact_server ! {factorial,N,self()},
  receive
    {factorial,N,Result} ->
      Result;
    {factorial,error} ->
      error
  end.

fact(1) ->
  1;
fact(N) when N>1 ->
  N*fact(N-1).

start() ->
  register(fact_server,spawn(fun () -> factorial_server(#{}) end)).

factorial_server(PidMap) ->
  process_flag(trap_exit,true), %% No me mates por favor!!
  receive
    {factorial,N,ClientPid} ->
      HijoPid = spawn_link(fun () ->
                     Result = fact(N),
                     ClientPid ! {factorial,N,Result}
                 end),
      factorial_server(maps:put(HijoPid,ClientPid,PidMap));
    
    %% Ha muerto un hijo, tenemos que mandar un mensaje al cliente
    {'EXIT',Pid,Reason} when Reason=/=normal ->
      io:format("child died with pid ~p map is ~p~n",[Pid,PidMap]),
      ClientPid = maps:get(Pid,PidMap),
      io:format("sending bad result to ~p~n",[ClientPid]),
      ClientPid ! {factorial,error},
      factorial_server(PidMap);

    Msg -> 
      io:format("got message ~p~n",[Msg]),
      factorial_server(PidMap)
  end.

      
             
