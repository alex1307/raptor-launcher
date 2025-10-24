-module(raptors_srv).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    status/0,
    list_services/0,
    is_service_running/1,
    start_service/1,
    stop_service/1,
    get_services/0
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
    case yml_utils:yml2map("devops/launcher.yml") of
        Map when is_map(Map) ->
            RaptorServicesMap = maps:get("raptor-services", Map),
            State = #{yml => RaptorServicesMap},
            {ok, State};
        {error, Reason} ->
            {stop, {error, {failed_to_load_yml, Reason}}};
        _ ->
            {stop, {error, invalid_yml_format}}
    end.

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
handle_call(get_services, _From, State) ->
    #{yml := RaptorServicesMap} = State,
    {reply, RaptorServicesMap, State};
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


-spec(status() -> term()).
status() ->
    gen_server:call(?MODULE, get_services).

-spec(list_services() -> [string()]).
list_services() ->
    Result = gen_server:call(?MODULE, {list_services}),
    case Result of
        % eqwalizer:fixme - List contains service names from YAML
        List when is_list(List) -> List;
        _ -> []
    end.

-spec(is_service_running(string()) -> boolean()).
is_service_running(ServiceName) ->
    % eqwalizer:fixme - returns boolean from raptors_utils
    gen_server:call(?MODULE, {is_service_running, ServiceName}).

-spec(start_service(string()) -> {ok, string()} | {error, string()}).
start_service(ServiceName) ->
    % eqwalizer:fixme - returns {ok, Msg} | {error, Msg} from raptors_utils
    gen_server:call(?MODULE, {start_service, ServiceName}).

-spec(stop_service(string()) -> {ok, string()} | {error, string()}).
stop_service(ServiceName) ->
    % eqwalizer:fixme - returns {ok, Msg} | {error, Msg} from raptors_utils
    gen_server:call(?MODULE, {stop_service, ServiceName}).  

-spec(get_services() -> map()).
get_services() ->
    % eqwalizer:fixme - returns map from state
    gen_server:call(?MODULE, get_services).
