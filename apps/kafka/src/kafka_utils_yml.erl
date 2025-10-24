-module(kafka_utils_yml).
-export([
    list_topics/1,
    describe_topic/2,
    create_topic/2,
    alter_topic/2,
    is_kafka_alive/1
]).


%% ----------------------------------------------------------------------------
%% Kafka utilities - YAML driven commands
%% ----------------------------------------------------------------------------

%% List topics
-spec list_topics(map()) -> {ok, [string()]} | {error, string()}.
list_topics(ConfigMap) ->
    TopicMap = maps:get("topics", ConfigMap, #{}),
    Keys = maps:keys(TopicMap),
    Topics = lists:filtermap(fun(K) -> 
        case maps:get(K, TopicMap) of
            V when is_list(V) -> {true, V};
            V when is_binary(V) -> {true, binary_to_list(V)};
            _ -> false
        end
    end, Keys),
    {ok, Topics}.

%% Describe topic
-spec describe_topic(map(), string()) -> {ok, string()} | {error, string()}.
describe_topic(ConfigMap, Topic) ->
    exec_kafka_cmd(ConfigMap, "describe_topic", #{
        "topic" => Topic
    }).

%% Create topic
-spec create_topic(map(), string()) -> {ok, string()} | {error, string()}.
create_topic(ConfigMap, Topic) ->
    exec_kafka_cmd(ConfigMap, "create_topic", #{
        "topic" => Topic
    }).

%% Alter topic config (retention)
-spec alter_topic(map(), string()) -> {ok, string()} | {error, string()}.
alter_topic(ConfigMap, Topic) ->
    Configs = maps:get("configs", ConfigMap),
    Ret = maps:get("retention", Configs),
    Vars = #{
        "topic" => Topic,
        "retention_ms" => integer_to_list(maps:get("ms", Ret)),
        "retention_bytes" => integer_to_list(maps:get("bytes", Ret))
    },
    exec_kafka_cmd(ConfigMap, "alter_topic", Vars).

%% Check if Kafka is alive (simple topic list)
-spec is_kafka_alive(map()) -> boolean().
is_kafka_alive(ConfigMap) ->
    case list_topics(ConfigMap) of
        {ok, Output} ->
            case unicode:characters_to_list(string:lowercase(Output)) of
                LowerOutput when is_list(LowerOutput) ->
                    not lists:member("error", LowerOutput);
                _ ->
                    false
            end;
        {error, _} ->
            false
    end.

%% ----------------------------------------------------------------------------
%% Internal helper
%% ----------------------------------------------------------------------------

-spec exec_kafka_cmd(map(), string(), map()) -> {ok, string()} | {error, string()}.
exec_kafka_cmd(ConfigMap, Key, Vars) ->
    Cmds = maps:get("commands", ConfigMap),
    CmdTemplate = maps:get(Key, Cmds),
    Container = maps:get("container", ConfigMap),
    Bootstrap = maps:get("bootstrap_server", ConfigMap),

    %% substitute placeholders
    Cmd1 = string:replace(CmdTemplate, "{{container}}", Container, all),
    Cmd2 = string:replace(Cmd1, "{{bootstrap}}", Bootstrap, all),
    Cmd2Flat = case unicode:characters_to_list(Cmd2) of
        Str when is_list(Str) -> Str;
        _ -> ""
    end,
    Cmd3 = substitute_topic_vars(Cmd2Flat, Vars),

    case cmd_utils:execute(Cmd3) of
        error -> {error, "Command execution failed"};
        Result -> Result
    end.

%% Substitute {{topic}}, {{retention_ms}}, {{retention_bytes}} if present
-spec substitute_topic_vars(string(), map()) -> string().
substitute_topic_vars(Cmd, Vars) ->
    Result = lists:foldl(
        fun({K, V}, Acc) ->
            KStr = ensure_string(K),
            VStr = ensure_string(V),
            AccStr = ensure_string(Acc),
            string:replace(AccStr, "{{" ++ KStr ++ "}}", VStr, all)
        end,
        Cmd,
        maps:to_list(Vars)
    ),
    ensure_string(Result).

-spec ensure_string(term()) -> string().
ensure_string(Val) when is_list(Val) -> 
    case io_lib:printable_unicode_list(Val) of
        true -> lists:flatten(io_lib:format("~ts", [Val]));
        false -> ""
    end;
ensure_string(Val) when is_binary(Val) -> binary_to_list(Val);
ensure_string(_) -> "".