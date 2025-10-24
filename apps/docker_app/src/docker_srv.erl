-module(docker_srv).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, 
    is_docker_running/0,
    is_container_running/1,
    start_container/1,
    stop_container/1,
    restart_container/1,
    start_compose/0,
    stop_compose/0, 
    restart_compose/0,
    start_docker_daemon/0,
    stop_docker_daemon/0,
    status/0
    ]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

is_docker_running() ->
    gen_server:call(?MODULE, is_docker_running).   

is_container_running(Container) ->
    gen_server:call(?MODULE, {is_container_running, Container}).

start_container(Container) ->
    gen_server:call(?MODULE, {start_container, Container}).
stop_container(Container) ->
    gen_server:call(?MODULE, {stop_container, Container}).
restart_container(Container) ->
    gen_server:call(?MODULE, {restart_container, Container}).
start_compose() ->
    gen_server:call(?MODULE, {start_compose}).
stop_compose() ->
    gen_server:call(?MODULE, {stop_compose}).
restart_compose() ->
    gen_server:call(?MODULE, {restart_compose}).
start_docker_daemon() ->
    gen_server:call(?MODULE, start).
stop_docker_daemon() ->
    gen_server:call(?MODULE, stop).
status() ->
    gen_server:call(?MODULE, status).
%%%===============================================================

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    case yml_utils:yml2map("devops/launcher.yml") of
        Map when is_map(Map) ->
            DockerMap = maps:get("docker", Map),
            State = #{yml => DockerMap},
            {ok, State};
        {error, Reason} ->
            {stop, {yml_error, Reason}};
        _ ->
            {stop, invalid_yml_format}
    end.

handle_call(is_docker_running, _From, State=#{yml := Yml}) ->
    IsRunning = docker_utils_yml:is_docker_alive(Yml),
    {reply, IsRunning, State};
handle_call({is_container_running, Container}, _From, State=#{yml := Yml}) ->
    IsRunning = docker_utils_yml:is_container_running(Yml, Container),
    {reply, IsRunning, State};
handle_call(start, _From, State=#{yml := Yml}) ->
    Result = docker_utils_yml:start_docker_daemon(Yml),
    {reply, Result, State};
handle_call(stop, _From, State=#{yml := Yml}) ->
    Result = docker_utils_yml:stop_docker_daemon(Yml),
    {reply, Result, State};
handle_call({start_container, Container}, _From, State=#{yml := Yml})  ->
    docker_utils_yml:start_container(Yml, Container),
    {reply, ok, State};
handle_call({stop_container, Container}, _From, State=#{yml := Yml})  ->
    docker_utils_yml:stop_container(Yml, Container),
    {reply, ok, State};
handle_call({restart_container, Container}, _From, State=#{yml := Yml})  ->
    docker_utils_yml:restart_container(Yml, Container),
    {reply, ok, State};
handle_call({start_compose}, _From, State=#{yml := Yml})  ->
    docker_utils_yml:start_compose(Yml),
    {reply, ok, State};
handle_call({stop_compose}, _From, State=#{yml := Yml})  ->
    docker_utils_yml:stop_compose(Yml),
    {reply, ok, State};
handle_call({restart_compose}, _From, State=#{yml := Yml})  ->
    docker_utils_yml:restart_compose(Yml),
    {reply, ok, State};
handle_call(status, _From, State = #{yml := Yml}) ->
    Status = docker_utils_yml:status(Yml),
    {reply, Status, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
