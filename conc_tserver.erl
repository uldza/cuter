-module(conc_tserver).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3,
  code_change/3, handle_info/2, handle_cast/2]).

%-compile([export_all]).

%% gen_server state datatype
-record(state, {
  super,
  procs,       %% List of Pids of Live Evaluator processes
  traces,
  ptree
}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Super]) ->
  process_flag(trap_exit, true),
  Traces = ets:new(?MODULE, [ordered_set, protected]),
  Ptree = ets:new(?MODULE, [bag, protected]),
  {ok, #state{super=Super, procs=[], traces=Traces, ptree=Ptree}}.
  
terminate(_Reason, State) ->
  Super = State#state.super,
  Traces = State#state.traces,
  Ptree = State#state.ptree,
  %% TODO
  %% reconstruct Process Tree and Traces Tree
  %%
    io:format("***** Traces *****~n"),
    report(Super, Traces, Ptree),
    io:format("******************~n"),
  %%
  %%
  ets:delete(Traces),
  ets:delete(Ptree),
  Super ! {self(), State},
  ok.

code_change(_OldVsn, State, _Extra) ->
  %% No change planned.
  {ok, State}.
  
handle_call({register_parent, Parent}, {From, _FromTag}, State) ->
  Procs = State#state.procs,
  Ptree = State#state.ptree,
  FromPid = 
    case is_atom(From) of
     true ->  whereis(From);
     false -> From
    end,
  monitor(process, FromPid),
  ets:insert(Ptree, {Parent, FromPid}),
  NewProcs = [FromPid|Procs],
  {reply, ok, State#state{procs=NewProcs}};
  
handle_call(terminate, _From, State) ->
  {stop, normal, stopped, State}.
  
handle_cast({trace, Who, Trace}, State) ->
  Traces = State#state.traces,
  case ets:lookup(Traces, Who) of
    [] ->
      ets:insert(Traces, {Who, [Trace]});
    [{Who, OldTrace}] ->
      ets:insert(Traces, {Who, [Trace | OldTrace]})
  end,
  {noreply, State}.

handle_info({'DOWN', _Ref, process, Who, normal}, State) ->
  Procs = State#state.procs,
  NewProcs = lists:delete(Who, Procs),
  case NewProcs of
    [] ->
      {stop, normal, State#state{procs=NewProcs}};
    _  ->
      {noreply, State#state{procs=NewProcs}}
  end;
  
handle_info({'DOWN', _Ref, process, Who, Reason}, State) ->
  Procs = State#state.procs,
  NewProcs = lists:delete(Who, Procs),
  %% TODO Kill all processes
  io:format("[conc_tserver]: ~p : ~p~n", [Who, Reason]),
  {stop, normal, State#state{procs=NewProcs}};
  
handle_info(Msg, State) ->
  %% Just outputting unexpected messages for now
  io:format("[conc_tserver]: Unexpected message ~p~n", [Msg]),
  {noreply, State}.

report([], _Traces, _Ptree) ->
  ok;
  
report(Super, Traces, Ptree) 
  when is_pid(Super) ->
    io:format("[conc_tserver]: Skipping Super ~p~n", [Super]),
    L = ets:lookup(Ptree, Super),
    Procs = lists:map(fun({_X, Y}) -> Y end, L),
    report(Procs, Traces, Ptree);
    
report([Proc|Procs], Traces, Ptree) ->
  [{Proc, Trace}] = ets:lookup(Traces, Proc),
  io:format("[conc_tserver]: Trace of ~p:~n~p~n", [Proc, lists:reverse(Trace)]),
  case ets:lookup(Ptree, Proc) of
    [] ->
      report(Procs, Traces, Ptree);
    L ->
      NewProcs = lists:map(fun({_X, Y}) -> Y end, L),
      report(Procs ++ NewProcs, Traces, Ptree)
  end.