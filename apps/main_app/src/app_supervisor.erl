%%%-------------------------------------------------------------------
%%% @author alexander.todorov@ayagasha.com
%%% @copyright (C) 2022, <AYAGASHA>
%%% @doc
%%%
%%% @end
%%% Created : 16. Dec 2022
%%%-------------------------------------------------------------------
-module(app_supervisor).
-author("alex").

-behaviour(supervisor).

%% API
-export([start_link/0, start_global/2]).

%% Supervisor callbacks
-export([init/1]).

-export([start_child/4, start_child/5]).
-export([terminate_child/1, processes/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_global(Name, Host) ->
  supervisor:start_link({global, {Name, Host}}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]}}
  | ignore | {error, Reason :: term()}).
init([]) ->
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = #{strategy => one_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},
  lager:info("[~p]:[~p] ~p <<<SYSTEM>>> Supervisor [~p] is started",
    [?MODULE, ?LINE, self(), ?MODULE]),
  {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
child(Name, Mod, Fun, Args, Opts)
  when is_atom(Name) andalso is_atom(Mod) andalso is_atom(Fun) andalso is_list(Args) andalso is_list(Opts)->
  #{id => Name,
    start => {Mod, Fun, Args, [Opts]},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [Mod]};


child(_Name, _Mod, _Fun, _Args, _Opts) ->
  error.


child(Name, Mod, Fun, Args)
  when is_atom(Name) andalso is_atom(Mod) andalso is_atom(Fun) andalso is_list(Args) ->
  #{id => Name,
    start => {Mod, Fun, Args},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [Mod]};


child(_Name, _Mod, _Fun, _Args) ->
  error.


start_child(Name, Mod, Fun, Args) ->
  case child(Name, Mod, Fun, Args) of
    error ->
      lager:error("[~p]:[~p] ~p <<<SYSTEM>>> Can't start process \n\t [{name: ~p}, {module, ~p}, {function: ~p}, {args, ~p}].",
        [?MODULE, ?LINE, self(), Name, Mod, Fun, Args]),
      error;
    Child ->
      lager:info("Starting child: ~p", [Child]),
      Res = supervisor:start_child(?MODULE, Child),
      lager:info("[~p]:[~p] ~p <<<SYSTEM>>> Process [~p] is started", [?MODULE, ?LINE, self(), Name]),
      lager:info("Response ~p", [Res]),

      Res
  end.

start_child(Name, Mod, Fun, Args, Options) ->
  case child(Name, Mod, Fun, Args, Options) of
    error ->
      lager:error("[~p]:[~p] ~p <<<SYSTEM>>> Can't start process \n\t [{name: ~p}, {module, ~p}, {function: ~p}, {args, ~p}].",
        [?MODULE, ?LINE, self(), Name, Mod, Fun, Args]),
      error;
    Child ->
      lager:info("Starting child: ~p", [Child]),
      Res = supervisor:start_child(?MODULE, Child),
      lager:info("[~p]:[~p] ~p <<<SYSTEM>>> Process [~p] is started", [?MODULE, ?LINE, self(), Name]),
      lager:info("Response ~p", [Res]),

      Res
  end.





terminate_child(Name) ->
  lager:info("[~p]:[~p] ~p <<<SYSTEM>>> Process [~p] is being terminated.",
    [?MODULE, ?LINE, self(), Name]),
  supervisor:terminate_child(?MODULE, Name),
  supervisor:delete_child(?MODULE, Name),
  lager:info("[~p]:[~p] ~p <<<SYSTEM>>> Process [~p] is terminated", [?MODULE, ?LINE, self(), Name]).

-spec(processes(Module :: module()) ->
  {ok, Children :: list()}).

processes(Module) ->
  lager:info("Start: ~p, Server: ~p", [Module, ?SERVER]),
  Children = supervisor:which_children(?SERVER),
  lager:info("Children: ~p", [Children]),
  Filtered = lists:filter(
    fun({_Id, _Child, _Type, Modules}) ->
      lists:any(fun(M) -> M == Module end, Modules)
    end,
    Children),
  {ok, lists:map(
    fun({_, Pid, _, _})
      -> Pid
    end,
    Filtered)}.
