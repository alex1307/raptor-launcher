-module(service_manager_srv).
-behaviour(gen_server).

%% API
-export([start_link/0, run/2, get_status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export_type([state/0]).

%% Service state record
-type service_status() :: ok | down | starting | error | disabled.
-record(service_state, {
    status      :: service_status(),
    updated_at  :: integer(),
    retries     :: non_neg_integer(),
    last_error  :: string(),
    critical    :: boolean(),
    meta        :: map()
}).

-type state() :: #{atom() => #service_state{}}.

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



-spec run(atom(), term()) -> ok.
run(Domain, Cmd) ->
    gen_server:cast(?MODULE, {run, Domain, Cmd}).

-spec get_status() -> term().
get_status() ->
    gen_server:call(?MODULE, get_state).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Now = erlang:system_time(millisecond),
    State = #{
        docker => #service_state{status = down, updated_at = Now, retries = 0, last_error = "", critical = true, meta = #{}},
        chrome => #service_state{status = down, updated_at = Now, retries = 0, last_error = "", critical = true, meta = #{}},
        postgres => #service_state{status = down, updated_at = Now, retries = 0, last_error = "", critical = false, meta = #{}},
        web_admin => #service_state{status = down, updated_at = Now, retries = 0, last_error = "", critical = false, meta = #{}},
        kafka => #service_state{status = down, updated_at = Now, retries = 0, last_error = "", critical = false, meta = #{}},
        raptor => #service_state{status = down, updated_at = Now, retries = 0, last_error = "", critical = true, meta = #{}},
        mobile_de => #service_state{status = down, updated_at = Now, retries = 0, last_error = "", critical = false, meta = #{}},
        crawler => #service_state{status = down, updated_at = Now, retries = 0, last_error = "", critical = false, meta = #{}}
    },
    {ok, State}.

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call({docker, status}, _From, State) ->
    lager:info("docker status"),
    %% example only — real check should delegate to docker_utils
    Now = erlang:system_time(millisecond),
    Old = maps:get(docker, State),
    case docker_utils_yml:is_docker_alive() of
        false ->
            {reply, down, Old#service_state{status = down, updated_at = Now, last_error = "ERR: Docker daemon NOT running"}};
        true ->
          {reply, ok, Old#service_state{status = down, updated_at = Now, last_error = "Docker daemon is running"}}
    end;

handle_call(docker_status, _From, State) ->
    %% example only — real check should delegate to docker_utils
    case docker_utils:status() of
        true ->  {reply, ok, State};
        false -> {reply, error, State}
    end;

handle_call({create_topic, TopicName}, _From, State) ->
    %% example only — real check should delegate to docker_utils
    case kafka_utils:create_topic(TopicName, 1, 1) of
        ok ->  {reply, ok, State};
        _ -> {reply, error, State}
    end;


handle_call({configure_topic, TopicName}, _From, State) ->
    %% example only — real check should delegate to docker_utils
    case kafka_utils:configure_kafka_topic(TopicName) of
        {ok, Config} ->  {reply, Config, State};
        _-> {reply, error, State}
    end;


handle_call({exec, _Domain, _Cmd}, _From, State) ->
    {reply, {error, unknown_command}, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.