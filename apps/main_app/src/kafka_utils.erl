-module(kafka_utils).
-export([
    configure_kafka_topic/1,
    create_topic/3,
    describe_topic/1
]).
-define(KAFKA_CONTAINER, 'kafka-server').
-define(BOOTSTRAP_SERVER, 'localhost:9092').
-define(KAFKA_CONFIG_LIST, [{'retention.ms', '86400000'}, {'retention.bytes', '104857600'}]).
-define(RETENTION_MS, "retention.ms").
-define(RETENTION_BYTES, "retention.bytes").
-define(RETENTION_PREFIX, "retention.").

-record(topic_config, {
    topic :: string(),
    retention_ms :: integer(),
    retention_bytes :: integer()
}).


-spec topic_exists(Topic :: string()) -> boolean().
topic_exists(Topic) -> 
    Cmd = io_lib:format("docker exec ~s kafka-topics.sh --list --bootstrap-server ~s", 
    [?KAFKA_CONTAINER, ?BOOTSTRAP_SERVER]),
    cmd_utils:execute(lists:flatten(Cmd)),
    case cmd_utils:execute(lists:flatten(Cmd)) of
        {error, _Reason} -> false;
        {ok, Output} ->
            case string:find(Output, Topic) of
                nomatch -> false;
                _ -> true
            end
    end.

-spec create_topic(Topic :: string(), Partitions :: integer(), Replicas :: integer()) -> ok | {error, string()}.
create_topic(Topic, Partitions, Replicas) ->
    Cmd = io_lib:format(
        "docker exec ~s kafka-topics.sh --create --topic ~s --partitions ~p --replication-factor ~p --bootstrap-server ~s",
        [?KAFKA_CONTAINER, Topic, Partitions, Replicas, ?BOOTSTRAP_SERVER]
    ),
    cmd_utils:execute(lists:flatten(Cmd)),
    case cmd_utils:execute(lists:flatten(Cmd)) of
        {ok, _Output} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec configure_kafka_topic(Topic :: string()) -> {ok, #topic_config{}} | error.
configure_kafka_topic(Topic) ->
    Exist = topic_exists(Topic),
    case Exist of
        true -> 
            lager:debug("Topic ~s already exists", [Topic]),
            describe_topic(Topic);
        false ->
            case create_topic(Topic, 1, 1) of
                ok -> 
                    lager:debug("Topic ~s created successfully", [Topic]),
                    alter_topic_config(Topic),
                    describe_topic(Topic);                    
                {error, Reason} ->
                    lager:debug("Failed to create topic ~s: ~p", [Topic, Reason]),
                    error
            end
    end.

-spec alter_topic_config(Topic :: string()) -> {ok, string()} | error.
alter_topic_config(Topic) ->
    Cmd  = [
        run_cmd(io_lib:format(
            "docker exec ~s kafka-configs.sh 
                --alter --bootstrap-server ~s 
                --entity-type topics 
                --entity-name ~s 
                --add-config ~s=~s 
                 grep -q 'Completed updating config' && echo OK",
            [?KAFKA_CONTAINER, ?BOOTSTRAP_SERVER, Topic, K, V]))
        || {K, V} <- ?KAFKA_CONFIG_LIST
    ],
    
    case cmd_utils:execute(lists:flatten(Cmd)) of
        {ok, "OK"} ->
            {ok, "OK"};
        _ ->
            error
    end.

-spec describe_topic(string()) -> {ok, #topic_config{}} | error.
describe_topic(Topic) ->
    Cmd = io_lib:format(
        "docker exec ~s kafka-configs.sh --describe --bootstrap-server ~s --entity-type topics --entity-name ~s",
        [?KAFKA_CONTAINER, ?BOOTSTRAP_SERVER, Topic]
    ),
    case cmd_utils:execute(lists:flatten(Cmd)) of
        {error, _Reason} -> error;
        {ok, Output} -> 
            Parsed = parse_kafka_config_output(Output),
            Config = parse_topic_config(Parsed, Topic),
            {ok, Config};
        _ ->
            error
    end.
    

-spec parse_kafka_config_output(string()) -> #{binary() => binary()}.
parse_kafka_config_output(Output) ->
    Lines = string:split(string:trim(Output), " ", all),
    Filtered = lists:filter(fun(Line) -> 
        string:find(Line, "=") =/= nomatch andalso 
        string:prefix(Line, ?RETENTION_PREFIX) =/= nomatch
    end, Lines),
    KVs = [string:split(string:trim(Line), "=", all)
       || Line <- Filtered,
          string:find(Line, "=") =/= nomatch],
    #{ list_to_binary(K) => list_to_binary(V) || [K, V] <- KVs }.

-spec run_cmd(CmdIoList :: iolist()) -> ok | {error, string()}.
run_cmd(CmdIoList) ->
    Cmd = lists:flatten(CmdIoList),
    cmd_utils:execute(Cmd).


-spec parse_topic_config(list(), string()) -> #topic_config{} | {error, string()}.
parse_topic_config(KVs, Name) ->
    RetMs = get_value(?RETENTION_MS, KVs),
    RetBytes = get_value(?RETENTION_BYTES, KVs),

    case {RetMs, RetBytes} of
        {undefined, undefined} ->
            {error, "no retention values found"};
        _ ->
            #topic_config{
                topic = Name,
                retention_ms = RetMs,
                retention_bytes = RetBytes
            }
    end.

-spec get_value(string(), list()) -> integer() | undefined.
get_value(Key, Map) when is_map(Map) ->
    BinKey = list_to_binary(Key),
    case maps:get(BinKey, Map, undefined) of
        undefined -> undefined;
        V when is_binary(V) ->
            try binary_to_integer(V) of
                I when is_integer(I) -> I
            catch
                _:_ -> undefined
            end;
        V when is_list(V) ->
            try list_to_integer(V) of
                I when is_integer(I) -> I
            catch
                _:_ -> undefined
            end;
        _ -> undefined
    end;
get_value(Key, KVs) when is_list(KVs) ->
    case lists:keyfind(Key, 1, KVs) of
        {_, V} ->
            try list_to_integer(V) of
                I when is_integer(I) -> I
            catch
                _:_ -> undefined
            end;
        false -> undefined
    end.