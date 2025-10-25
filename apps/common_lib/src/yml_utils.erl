-module(yml_utils).

%% ----------------------------------------------------------------------------
%% YAML utilities
%% ----------------------------------------------------------------------------
-export([
    yml2map/0,
    load_yaml/0,
    to_map/1
]).

%% ----------------------------------------------------------------------------
%% @doc
%% Utility module for reading and converting YAML files into nested Erlang maps.
%% Supports recursive conversion and safe error handling.
%% ----------------------------------------------------------------------------

%% Load YAML file and return as list of maps
-define(YAML_PATH, "LAUNCHER_YAML_PATH").
-spec load_yaml() -> {ok, term()} | {error, term()}.
load_yaml() ->
    try
        FilePath = os:getenv(?YAML_PATH, "/home/matkat/launcher-app/devops/launcher.yml"),
        YamlData = yamerl_constr:file(FilePath),
        {ok, YamlData}
    catch
        _:Reason ->
            {error, Reason}
    end.

%% Convert YAML parsed structure recursively to a map
-spec to_map(term()) -> map() | list() | term().
to_map(List) when is_list(List) ->
    case lists:all(fun(E) -> is_tuple(E) andalso tuple_size(E) == 2 end, List) of
        true -> maps:from_list([{K, to_map(V)} || {K, V} <- List]);
        false -> [to_map(E) || E <- List]
    end;
to_map(Other) ->
    Other.

%% Load YAML file and convert to a map
-spec yml2map() -> map() | list() | term() | {error, term()}.
yml2map() ->
    application:ensure_all_started(yamerl),
    case load_yaml() of
        {ok, [Yaml | _]} -> to_map(Yaml);
        {error, Reason} -> {error, Reason}
    end.