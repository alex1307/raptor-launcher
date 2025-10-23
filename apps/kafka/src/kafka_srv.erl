-module(kafka_srv).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([is_kafka_running/0, list_kafka_topics/0, describe_kafka_topic/1, create_kafka_topic/1, alter_kafka_topic/1, configure_all_topics/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Map = yml_utils:yml2map("devops/launcher.yml"),
    KafkaMap = maps:get("kafka", Map),
    State = #{yml => KafkaMap},
    lager:info("Kafka service initialized with config: ~p", [KafkaMap]),
    {ok, State}.

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
    gen_server:call(?MODULE, {is_kafka_alive}).

-spec list_kafka_topics() -> [string()].
list_kafka_topics() ->
    gen_server:call(?MODULE, {list_topics}).

-spec describe_kafka_topic(string()) -> map() | {error, term()}.
describe_kafka_topic(Topic) ->
    gen_server:call(?MODULE, {describe_topic, Topic}).

-spec create_kafka_topic(string()) -> ok | {error, term()}.
create_kafka_topic(Topic) ->
    gen_server:call(?MODULE, {create_topic, Topic}).

-spec alter_kafka_topic(string()) -> ok | {error, term()}.
alter_kafka_topic(Topic) ->
    gen_server:call(?MODULE, {alter_topic, Topic}).

-spec configure_all_topics() -> ok | {error, term()}.
configure_all_topics() ->
    {ok, Topics} = list_kafka_topics(),
    Configured = lists:map(fun(T) -> 
        case describe_kafka_topic(T) of
            {error, _} -> create_kafka_topic(T);
            Result -> Result
        end 
    end, Topics),
    IsOK = lists:all(fun(P) -> 
        case P of 
            {ok, _} -> true;
            _ -> false 
        end
    end, Configured),
    case IsOK of
        true -> ok;
        false -> {error, misconfigured_topics}
    end.