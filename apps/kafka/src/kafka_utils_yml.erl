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
-spec list_topics(map()) -> {ok, []} | {error, string()}.
list_topics(ConfigMap) ->
    TopicMap = maps:get("topics", ConfigMap, []),
    Keys = maps:keys(TopicMap),
    Topics = lists:map(fun(K) -> maps:get(K, TopicMap) end, Keys),
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
            not lists:member("error", string:lowercase(Output));
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
    Cmd3 = substitute_topic_vars(Cmd2, Vars),

    cmd_utils:execute(Cmd3).

%% Substitute {{topic}}, {{retention_ms}}, {{retention_bytes}} if present
-spec substitute_topic_vars(string(), map()) -> string().
substitute_topic_vars(Cmd, Vars) ->
    lists:foldl(
        fun({K, V}, Acc) ->
            string:replace(Acc, "{{" ++ K ++ "}}", V, all)
        end,
        Cmd,
        maps:to_list(Vars)
    ).