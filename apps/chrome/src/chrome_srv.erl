-module(chrome_srv).
-behavior(gen_server).
-export([
    start_link/0,
    stop/1,
    status/1,
    start_chrome/0,
    stop_chrome/0,
    is_chrome_running/0,
    status/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ----------------------------------------------------------------------------
%% Chrome service - GenServer wrapper
%% ----------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
-spec stop(pid()) -> ok.
stop(Pid) ->
    ok = gen_server:call(Pid, stop),
    ok.
-spec status(pid()) -> {ok, map()} | {error, string()}.
status(Pid) ->
    gen_server:call(Pid, status).


-spec start_chrome() -> ok.
start_chrome() ->
    ok = gen_server:call(?MODULE, start_chrome),
    ok.

-spec stop_chrome() -> ok.
stop_chrome() ->
    ok = gen_server:call(?MODULE, stop_chrome),
    ok.

-spec is_chrome_running() -> boolean().
is_chrome_running() ->
    Result = gen_server:call(?MODULE, is_chrome_running),
    case Result of
        true -> true;
        _ -> false
    end.

-spec status() -> {ok, map()} | {error, string()}.
status() ->
    Result = gen_server:call(?MODULE, chrome_status),
    Result.

%% GenServer callbacks
init([]) ->
    case yml_utils:yml2map() of
        Map when is_map(Map) ->
            ChromeMap = maps:get("chrome", Map),
            State = #{yml => ChromeMap},
            {ok, State};
        _ ->
            {stop, {error, invalid_yml_config}}
    end.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(chrome_status, _From, State=#{yml := Yml}) ->
    Result = chrome_utils:status(Yml),
    {reply, Result, State};
handle_call(is_chrome_running, _From, State=#{yml := Yml}) ->
    IsRunning = chrome_utils:is_chrome_running(Yml),
    {reply, IsRunning, State};
handle_call(start_chrome, _From, State=#{yml := Yml}) ->
    chrome_utils:start_chrome(Yml),
    {reply, ok, State};
handle_call(stop_chrome, _From, State=#{yml := Yml}) ->    
    chrome_utils:stop_chrome(Yml),
    {reply, ok, State};
handle_call(_, _From, State) ->
    {reply, {error, unknown_command}, State}.

%%%===================================================================


handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

