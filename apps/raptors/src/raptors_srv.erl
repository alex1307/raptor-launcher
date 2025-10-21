-module(raptors_srv).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    status/0,
    list_services/0,
    is_service_running/1,
    start_service/1,
    stop_service/1
]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).   

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------
init([]) ->
    Map = yml_utils:yml2map("devops/launcher.yml"),
    RaptorServicesMap = maps:get("raptor-services", Map),
    State = #{yml => RaptorServicesMap},
    lager:info("Raptor services initialized with config: ~p", [RaptorServicesMap]),
    {ok, State}.

handle_call({list_services}, _From, State) ->
    #{yml := RaptorServicesMap} = State,
    {reply, raptors_utils:list_services(RaptorServicesMap), State};
handle_call({is_service_running, ServiceName}, _From, State) ->
    #{yml := RaptorServicesMap} = State,
    Result = raptors_utils:is_service_running(RaptorServicesMap, ServiceName),
    {reply, Result, State};
handle_call({start_service, ServiceName}, _From, State) ->
    #{yml := RaptorServicesMap} = State,
    Result = raptors_utils:start_service(RaptorServicesMap, ServiceName),
    {reply, Result, State};
handle_call({stop_service, ServiceName}, _From, State) ->
    #{yml := RaptorServicesMap} = State,
    Result = raptors_utils:stop_service(RaptorServicesMap, ServiceName),
    {reply, Result, State};
handle_call({status, ServiceName}, _From, State) ->
    #{yml := RaptorServicesMap} = State,
    IsRunning = raptors_utils:is_service_running(RaptorServicesMap, ServiceName),
    Status = if IsRunning -> "running"; true -> "stopped" end,
    {reply, Status, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


-spec(status() -> string()).
status() ->
    gen_server:call(?MODULE, {status}).

-spec(list_services() -> [string()]).
list_services() ->
    gen_server:call(?MODULE, {list_services}).

-spec(is_service_running(string()) -> boolean()).
is_service_running(ServiceName) ->
    gen_server:call(?MODULE, {is_service_running, ServiceName}).

-spec(start_service(string()) -> {ok, string()} | {error, string()}).
start_service(ServiceName) ->
    gen_server:call(?MODULE, {start_service, ServiceName}).

-spec(stop_service(string()) -> {ok, string()} | {error, string()}).
stop_service(ServiceName) ->
    gen_server:call(?MODULE, {stop_service, ServiceName}).  