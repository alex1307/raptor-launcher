-module(yml_utils).

%% ----------------------------------------------------------------------------
%% YAML utilities
%% ----------------------------------------------------------------------------
-export([
    yml2map/1,
    load_yaml/1,
    to_map/1
]).

%% ----------------------------------------------------------------------------
%% @doc
%% Utility module for reading and converting YAML files into nested Erlang maps.
%% Supports recursive conversion and safe error handling.
%% ----------------------------------------------------------------------------

%% Load YAML file and return as list of maps
-spec load_yaml(string()) -> {ok, term()} | {error, term()}.
load_yaml(FilePath) ->
    try
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
-spec yml2map(string()) -> map() | list() | term() | {error, term()}.
yml2map(FilePath) ->
    application:ensure_all_started(yamerl),
    case load_yaml(FilePath) of
        {ok, [Yaml | _]} -> to_map(Yaml);
        {error, Reason} -> {error, Reason}
    end.