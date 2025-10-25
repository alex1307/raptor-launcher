-module(kafka_srv).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([is_kafka_running/0, list_kafka_topics/0, describe_kafka_topic/1, create_kafka_topic/1, alter_kafka_topic/1, configure_all_topics/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    case yml_utils:yml2map() of
        Map when is_map(Map) ->
            KafkaMap = maps:get("kafka", Map),
            State = #{yml => KafkaMap},
            {ok, State};
        {error, Reason} ->
            {stop, {yml_parse_error, Reason}};
        _ ->
            {stop, invalid_yml_format}
    end.

handle_call({list_topics}, _From, State) ->
    #{yml := KafkaMap} = State,
    {reply, kafka_utils_yml:list_topics(KafkaMap), State};
handle_call({describe_topic, Topic}, _From, State) ->
    #{yml := KafkaMap} = State,
    {reply, kafka_utils_yml:describe_topic(KafkaMap, Topic), State};
handle_call({create_topic, Topic}, _From, State) ->
    #{yml := KafkaMap} = State,
    {reply, kafka_utils_yml:create_topic(KafkaMap, Topic), State};
handle_call({alter_topic, Topic}, _From, State) ->
    #{yml := KafkaMap} = State,
    {reply, kafka_utils_yml:alter_topic(KafkaMap, Topic), State};
handle_call({is_kafka_alive}, _From, State) ->
    #{yml := KafkaMap} = State,
    {reply, kafka_utils_yml:is_kafka_alive(KafkaMap), State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%% -------------------------------------------------------------------
%%% Kafka service
%%% Provides functionality to interact with Kafka broker
%%% -------------------------------------------------------------------

-spec is_kafka_running() -> boolean().
is_kafka_running() ->
    Result = gen_server:call(?MODULE, {is_kafka_alive}),
    case Result of
        true -> true;
        false -> false;
        _ -> false
    end.

-spec list_kafka_topics() -> [string()].
list_kafka_topics() ->
    Result = gen_server:call(?MODULE, {list_topics}),
    case Result of
        {ok, Topics} when is_list(Topics) -> 
            % eqwalizer:fixme - Topics are strings from YAML config
            Topics;
        _ -> []
    end.

-spec describe_kafka_topic(string()) -> map() | {error, term()}.
describe_kafka_topic(Topic) ->
    Result = gen_server:call(?MODULE, {describe_topic, Topic}),
    case Result of
        Map when is_map(Map) -> Map;
        {error, _} = Error -> Error;
        _ -> {error, invalid_response}
    end.

-spec create_kafka_topic(string()) -> ok | {error, term()}.
create_kafka_topic(Topic) ->
    Result = gen_server:call(?MODULE, {create_topic, Topic}),
    case Result of
        {ok, _Msg} -> ok;  % Success - normalize to ok
        {error, _} = Error -> Error;
        _ -> {error, invalid_response}
    end.

-spec alter_kafka_topic(string()) -> ok | {error, term()}.
alter_kafka_topic(Topic) ->
    Result = gen_server:call(?MODULE, {alter_topic, Topic}),
    case Result of
        {ok, _Msg} -> ok;  % Success - normalize to ok
        {error, _} = Error -> Error;
        _ -> {error, invalid_response}
    end.

-spec configure_all_topics() -> ok | {error, term()}.
configure_all_topics() ->
    Topics = list_kafka_topics(),  % Returns [string()]
    lager:info("kafka_srv: configuring ~p topics", [length(Topics)]),
    
    Results = lists:map(fun(T) -> 
        % eqwalizer:fixme - T is string from list_kafka_topics()
        lager:debug("kafka_srv: checking topic ~s", [T]),
        case describe_kafka_topic(T) of
            % eqwalizer:fixme
            {error, _Reason} -> 
                lager:info("kafka_srv: topic ~s does not exist, creating...", [T]),
                case create_kafka_topic(T) of
                    ok -> 
                        lager:info("kafka_srv: topic ~s created successfully", [T]),
                        ok;
                    {error, CreateErr} = Err ->
                        lager:error("kafka_srv: failed to create topic ~s: ~p", [T, CreateErr]),
                        Err
                end;
            _Map when is_map(_Map) -> 
                lager:debug("kafka_srv: topic ~s already exists", [T]),
                ok;
            Other ->
                lager:warning("kafka_srv: unexpected describe result for topic ~s: ~p", [T, Other]),
                {error, {unexpected_describe_result, Other}}
        end 
    end, Topics),
    
    % Check if all topics are OK
    Errors = [R || R <- Results, R =/= ok],
    case Errors of
        [] -> 
            lager:info("kafka_srv: all ~p topics configured successfully", [length(Topics)]),
            ok;
        _ -> 
            lager:error("kafka_srv: failed to configure topics, errors: ~p", [Errors]),
            {error, {misconfigured_topics, Errors}}
    end.