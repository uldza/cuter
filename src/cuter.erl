%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter).

-export([run/3, run/4, run/5, run_once/4, run_once/5]).

-export_type([mod/0, input/0, erroneous_inputs/0]).

-include("include/cuter_macros.hrl").

-type mod() :: atom().  % a subtype of module()
-type input() :: [any()].
-type erroneous_inputs() :: [input()].

-define(ZERO, 0).
-define(ONE,  1).
-type loop_limit() :: ?ZERO | ?ONE | inf.

-record(conf, {
  mod           :: mod(),
  func          :: atom(),
  dataDir       :: file:filename(),
  depth         :: pos_integer(),
  no            :: integer(),
  scheduler     :: pid(),
  stored_mods   :: cuter_analyzer:stored_modules(),
  tags_added_no :: integer(),
  pmatch        :: boolean()
}).
-type configuration() :: #conf{}.

%% Runtime Options
-define(FULLY_VERBOSE_EXEC_INFO, fully_verbose_execution_info).
-define(ENABLE_PMATCH, enable_pmatch).

-type option() :: {basedir, file:filename()}
                | ?FULLY_VERBOSE_EXEC_INFO
                | ?ENABLE_PMATCH
                .


-spec run_once(mod(), atom(), input(), pos_integer()) -> erroneous_inputs().
run_once(M, F, As, Depth) ->
  run_once(M, F, As, Depth, []).

-spec run_once(mod(), atom(), input(), pos_integer(), [option()]) -> erroneous_inputs().
run_once(M, F, As, Depth, Options) ->
  Conf = initialize_app(M, F, As, Depth, Options),
  loop(Conf, ?ONE).

-spec run(mod(), atom(), input()) -> erroneous_inputs().
run(M, F, As) ->
  run(M, F, As, ?DEFAULT_DEPTH).

-spec run(mod(), atom(), input(), pos_integer()) -> erroneous_inputs().
run(M, F, As, Depth) ->
  run(M, F, As, Depth, []).

-spec run(mod(), atom(), input(), pos_integer(), [option()]) -> erroneous_inputs().
run(M, F, As, Depth, Options) ->
  Conf = initialize_app(M, F, As, Depth, Options),
  loop(Conf, inf).

-spec loop(configuration(), loop_limit()) -> erroneous_inputs().
loop(Conf, ?ZERO) -> stop(Conf);
loop(Conf, Lmt) ->
  Scheduler = Conf#conf.scheduler,
  case cuter_scheduler_maxcover:request_input(Scheduler) of
    empty -> stop(Conf);
    {Ref, As, StoredMods, TagsN} ->
      No = Conf#conf.no + 1,
      Conf_n = Conf#conf{no = No, stored_mods = StoredMods, tags_added_no = TagsN},
      case concolic_execute(Conf_n, Ref, As) of
        cuter_error ->
          stop(Conf_n);
        Info ->
          ok = cuter_scheduler_maxcover:store_execution(Scheduler, Ref, Info),
          loop(Conf_n, tick(Lmt))
      end
  end.

-spec tick(inf) -> inf
       ; (?ONE) -> ?ZERO.
tick(inf) -> inf;
tick(?ONE) -> ?ZERO.

-spec initialize_app(mod(), atom(), input(), pos_integer(), [option()]) -> configuration().
initialize_app(M, F, As, Depth, Options) ->
  BaseDir = set_basedir(Options),
  process_flag(trap_exit, true),
  error_logger:tty(false),  %% disable error_logger
  SchedPid = cuter_scheduler_maxcover:start(?PYTHON_CALL, Depth, As),
  ok = cuter_pp:start(reporting_level(Options)),
  cuter_pp:mfa({M, F, length(As)}),
  #conf{mod = M,
        func = F,
        no = 1,
        depth = Depth,
        dataDir = cuter_lib:get_tmp_dir(BaseDir),
        scheduler = SchedPid,
        stored_mods = orddict:new(),
        tags_added_no = 0,
        pmatch = lists:member(?ENABLE_PMATCH, Options)}.

-spec stop(configuration()) -> erroneous_inputs().
stop(Conf) ->
  Erroneous = cuter_scheduler_maxcover:stop(Conf#conf.scheduler),
  cuter_pp:errors_found(Erroneous),
  cuter_pp:stop(),
  cuter_lib:clear_and_delete_dir(Conf#conf.dataDir),
  Erroneous.

%% Set app parameters.
-spec set_basedir([option()]) -> file:filename().
set_basedir([]) -> {ok, CWD} = file:get_cwd(), CWD;
set_basedir([{basedir, BaseDir}|_]) -> BaseDir;
set_basedir([_|Rest]) -> set_basedir(Rest).

-spec reporting_level([option()]) -> cuter_pp:pp_level().
reporting_level(Options) ->
  Default = cuter_pp:default_reporting_level(),
  case lists:member(?FULLY_VERBOSE_EXEC_INFO, Options) of
    false -> Default;
    true  -> cuter_pp:fully_verbose_exec_info(Default)
  end.

%% ------------------------------------------------------------------
%% Concolic Execution
%% ------------------------------------------------------------------

-spec concolic_execute(configuration(), cuter_scheduler_maxcover:exec_handle(), input()) -> cuter_analyzer:info() | cuter_error.
concolic_execute(Conf, Ref, Input) ->
  cuter_pp:input(Ref, Input),
  BaseDir = Conf#conf.dataDir,
  DataDir = cuter_lib:get_data_dir(BaseDir, Conf#conf.no),
  TraceDir = cuter_lib:get_trace_dir(DataDir),  % Directory to store process traces
  M = Conf#conf.mod,
  F = Conf#conf.func,
  Depth = Conf#conf.depth,
  StoredMods = Conf#conf.stored_mods,
  TagsN = Conf#conf.tags_added_no,
  WithPmatch = Conf#conf.pmatch,
  IServer = cuter_iserver:start(M, F, Input, TraceDir, Depth, StoredMods, TagsN, WithPmatch),
  retrieve_info(IServer, Ref, DataDir).

-spec retrieve_info(pid(), cuter_scheduler_maxcover:exec_handle(), file:filename()) -> cuter_analyzer:info() | cuter_error.
retrieve_info(IServer, Ref, DataDir) ->
  case wait_for_execution(IServer) of
    {ok, ExStatus, Info} ->
      cuter_pp:execution_status(Ref, ExStatus),
      cuter_pp:execution_info(Ref, Info),
      case cuter_analyzer:get_result(ExStatus) of
        internal_error -> cuter_error;
        ExResult ->
          Mappings = cuter_analyzer:get_mapping(Info),
          Traces = cuter_analyzer:get_traces(Info),
          Int = cuter_analyzer:get_int_process(Info),
          Tags = cuter_analyzer:get_tags(Info),
          StoredMods = cuter_analyzer:get_stored_modules(Info),
          TagsN = cuter_analyzer:get_no_of_tags_added(Info),
          RawInfo = cuter_analyzer:mk_raw_info(Mappings, ExResult, Traces, Int, DataDir, Tags, StoredMods, TagsN),
          AnalyzedInfo = cuter_analyzer:process_raw_execution_info(RawInfo),
          cuter_pp:path_vertex(Ref, cuter_analyzer:pathVertex_of_info(AnalyzedInfo)),
          cuter_pp:flush(Ref),
          AnalyzedInfo
      end;
    {error, Why} ->
      R = {internal_error, iserver, node(), Why},
      cuter_pp:execution_status(Ref, R),
      cuter_pp:flush(Ref),
      cuter_error
  end.

-spec wait_for_execution(pid()) -> {ok, cuter_iserver:execution_status(), orddict:orddict()} | {error, any()}.
wait_for_execution(IServer) ->
  receive
    {IServer, ExStatus, Info} ->
      ok = wait_for_iserver(IServer),
      {ok, ExStatus, Info};
    {'EXIT', IServer, Why} ->
      {error, Why}
  end.

-spec wait_for_iserver(pid()) -> ok | not_ok.
wait_for_iserver(IServer) ->
  receive {'EXIT', IServer, normal} -> ok
  after 10000 -> not_ok
  end.

