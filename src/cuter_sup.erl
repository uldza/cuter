%%%-------------------------------------------------------------------
%% @doc
%% cuter's top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(cuter_sup).
-behaviour(supervisor).

%% API
-export([start_link/0,
         worker/1,
         supervisor/2]).

%% Supervisor callbacks
-export([init/1]).


%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Define supervisor configuration given supervisor module and name.
%% @end
%%--------------------------------------------------------------------
-spec supervisor(Mod::atom(), Name::atom()) -> {Name::atom(),
                                                {Mod::atom(), start_link,
                                                 Args::list()},
                                                permanent,
                                                infinity,
                                                supervisor,
                                                [Mod::atom()]}.
supervisor(Mod, Name) when is_atom(Mod) andalso is_atom(Name) ->
    {Name, {Mod, start_link, [Name]},
     permanent, infinity, supervisor, [Mod]}.

%%--------------------------------------------------------------------
%% @doc
%% Define worker configuration given worker module.
%% @end
%%--------------------------------------------------------------------
-spec worker(Mod::atom()) -> {Name::atom(),
                              {Mod::atom(), start_link,
                               Args::list()},
                               permanent,
                               infinity,
                               worker,
                               [Mod::atom()]}.

worker(Mod) when is_atom(Mod) ->
    {Mod, {Mod, start_link, []},
    permanent, infinity, worker, [Mod]}.

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/0,
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------

init([]) ->
    Children = [],
    %% Allow 5 restarts in 3 seconds
    {ok, {{one_for_one, 5, 3}, Children}}.


%%====================================================================
%% Internal functions
%%====================================================================
