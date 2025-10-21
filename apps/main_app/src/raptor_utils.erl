%%% ==============================================================
%%%  raptor_utils.erl
%%%  --------------------------------------------------------------
%%%  Utility module for controlling the Raptor ecosystem services.
%%%  Provides unified start/stop/status functions for:
%%%    • crawler-ui
%%%    • mobile_de scraper
%%%    • raptor-core service
%%%
%%%  All commands are executed via cmd_utils:execute/1 with
%%%  consistent {ok, _} | {error, _} responses and proper logging.
%%%  The module automatically resolves script paths and supports
%%%  cross-platform behavior (macOS / Linux).
%%% ==============================================================

-module(raptor_utils).
-export([check_config/0, start/2, stop/1, is_running/2]).

%% ---------------------------------------------------------------
%% API
%% ---------------------------------------------------------------

-define(NOT_FOUND, "not_found").
-define(CRAWLER_HOME, os:getenv("CRAWLER_HOME", ?NOT_FOUND)).
-define(MOBILE_DE_HOME, os:getenv("MOBILE_DE_HOME", ?NOT_FOUND)).
-define(RAPTOR_HOME, os:getenv("RAPTOR_HOME", ?NOT_FOUND)).
-define(START_CRAWLER, os:getenv("START_CRAWLER", ?NOT_FOUND)).
-define(START_MOBILE_DE, os:getenv("START_MOBILE_DE", ?NOT_FOUND)).
-define(STOP_MOBILE_DE, os:getenv("STOP_MOBILE_DE", ?NOT_FOUND)).
-define(START_RAPTOR, os:getenv("START_RAPTOR", ?NOT_FOUND)).
-define(STOP_RAPTOR, os:getenv("STOP_RAPTOR", ?NOT_FOUND)).
-define(STOP_CRAWLER, os:getenv("STOP_CRAWLER", ?NOT_FOUND)).
-define(DATABASE_URL, os:getenv("DATABASE_URL", ?NOT_FOUND)).
-define(SERVICES, [crawler, mobile_de, raptor]).
-define(CRAWLER_SOURCES, ["autouncle.de", "autohaus.ro", "autouncle.ch", "autouncle.it", "autouncle.fr", "autouncle.nl", "autouncle.pl", "mobile.bg"]).

-spec check_config() -> ok|error.
check_config() ->
    EnvVars = [
        {crawler_home, ?CRAWLER_HOME},
        {mobile_de_home, ?MOBILE_DE_HOME},
        {raptor_home, ?RAPTOR_HOME},
        {start_crawler, ?START_CRAWLER},
        {start_mobile_de, ?START_MOBILE_DE},
        {stop_mobile_de, ?STOP_MOBILE_DE},
        {start_raptor, ?START_RAPTOR},
        {stop_raptor, ?STOP_RAPTOR},
        {stop_crawler, ?STOP_CRAWLER},
        {database_url, ?DATABASE_URL}
    ],
    Missing = [Name || {Name, Val} <- EnvVars, Val =:= ?NOT_FOUND],
    case Missing of
        [] ->
            lager:info("Raptor Utils configuration check passed."),
            ok;
        _ ->
            lager:error("Raptor Utils configuration missing variables: ~p", [Missing]),
            {error, Missing}
    end.
-spec start(atom(), string()) -> {ok, started} | {error, term()}.
start(crawler, Source) ->
    Cmd = start_cmd(crawler, Source),
    lager:info("Starting ~p ...", [crawler]),
    case cmd_utils:execute(Cmd) of
        {ok, _} -> {ok, started};
        Error -> Error
    end;

start(mobile_de, _Source) ->
    lager:info("Starting ~p ...", [mobile_de]),
    lager:info("Starting ~p ...", [?START_MOBILE_DE]),
    lager:info("MOBILE_DE_HOME ~p ...", [?MOBILE_DE_HOME]),
    case cmd_utils:execute(?START_MOBILE_DE) of
        {ok, _} -> {ok, started};
        Error -> lager:error("Error starting mobile_de: ~p", [Error]), 
            Error
    end;

start(raptor, _Source) ->
    Cmd = start_cmd(raptor, _Source),
    lager:info("Starting ~p ...", [raptor]),
    case cmd_utils:execute(Cmd) of
        {ok, _} -> {ok, started};
        Error -> Error
    end.


%---------------------------------------------------------------
%% Stop service
%%% ---------------------------------------------------------------

-spec stop(atom()) -> {ok, stopped} | {error, term()}.
stop(Service) ->
    Cmd = stop_cmd(Service),
    lager:info("Stopping ~p ...", [Service]),
    case cmd_utils:execute(Cmd) of
        {ok, _} -> {ok, stopped};
        Error -> Error
    end.

%---------------------------------------------------------------
%% Status is running check
%%% ---------------------------------------------------------------

-spec is_running(atom(), string()) -> boolean() | not_implemented.
is_running(mobile_de, Source) ->
    Cmd = status_cmd(mobile_de, Source),
    lager:info("Checking if mobile_de is running...Cmd: ~p", [Cmd]),
    case cmd_utils:execute(io_lib:format(Cmd, [])) of
        {ok, Output} ->
            lager:info("mobile_de status check output: ~p", [Output]),
            string:find(Output, "not found") =:= nomatch;
        _ -> false
    end;
is_running(crawler, Source) ->
    Cmd = status_cmd(crawler, Source),
    case cmd_utils:execute(Cmd) of
        {ok, Output} ->
            string:find(Output, "not found") =:= nomatch;
        _ -> false
    end;

is_running(raptor, Source) ->
    Cmd = status_cmd(raptor, Source),
    case cmd_utils:execute(Cmd) of
        {ok, Output} ->
            string:find(Output, "not found") =:= nomatch;
        _ -> false
    end;
is_running(_, _) -> not_implemented.


%% ---------------------------------------------------------------
%% Private command generators
%% ---------------------------------------------------------------
-spec(start_cmd(atom(), string()) -> string() | not_implemented).
start_cmd(crawler, Source) -> 
    case string:is_empty(Source) of
        true -> io_lib:format("nohup ~s 2>&1 &", [?START_CRAWLER]);
        false -> io_lib:format("nohup ~s --source ~s 2>&1 &", [?START_CRAWLER, Source])
    end;

start_cmd(mobile_de, _Source) ->
    lager:info("Using START_MOBILE_DE: ~p", [?START_MOBILE_DE]),
    ?START_MOBILE_DE;
start_cmd(raptor, _Source) ->
    ?START_RAPTOR;
start_cmd(_, _) ->
    lager:error("start_cmd: Unknown service"),
    not_implemented.

%---------------------------------------------------------------
%% Stop command generators
%%% ---------------------------------------------------------------

-spec(stop_cmd(atom()) -> string() | not_implemented).
stop_cmd(mobile_de) ->
    ?STOP_MOBILE_DE;

stop_cmd(raptor) ->
    ?STOP_RAPTOR;

stop_cmd(crawler) ->
    ?STOP_CRAWLER.

%%% ---------------------------------------------------------------
%% Status command generators
%%% ---------------------------------------------------------------

-spec(status_cmd(atom(), string()) -> string() | not_implemented).
status_cmd(crawler, source) ->
    case string:is_empty(source) of
        true -> "pgrep -fa 'crawler' || echo 'not found'";
        false -> io_lib:format("pgrep -fa 'crawler --~s' || echo 'not found'", [source])
    end;
    
status_cmd(mobile_de, _source) ->
    "pgrep -fa 'mobile_de' || echo 'not found'";

status_cmd(raptor, source) ->
    io_lib:format("pgrep -fa 'launcher.js --~s' || echo 'not found'", [source]);

status_cmd(_, _) ->
    not_implemented.