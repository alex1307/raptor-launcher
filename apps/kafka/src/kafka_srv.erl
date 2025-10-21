-module(kafka_srv).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Map = yml_utils:yml2map("/Users/matkat/Software/Erlang/raptor-launcher/devops/launcher.yml"),
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
