-module(cmd_utils).
-export([os_cmd_with_timeout/2, execute/1]).

-define(ERROR, "error").

%% Execute shell command with timeout (e.g. 5000 ms)
-spec os_cmd_with_timeout(string(), non_neg_integer()) -> string() | {error, timeout}.
os_cmd_with_timeout(Cmd, Timeout) ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn(fun() ->
        Result = os:cmd(Cmd),
        Parent ! {Ref, Result}
    end),
    receive
        {Ref, Output} ->
            Output
    after Timeout ->
        exit(Pid, kill),
        {error, timeout}
    end.


-spec execute(Cmd :: string()) -> {ok, string()} | {error, string()} | error.
execute(Cmd) when is_list(Cmd) ->
    case cmd_utils:os_cmd_with_timeout(Cmd, 10000) of
        {error, timeout} ->
            {error, "command timeout"};

        Output when is_list(Output) ->
            Lower = string:lowercase(Output),

            %% Universal success markers — work for Docker, Chrome, Kafka, etc.
            SuccessMarkers = [
                "already exists", "completed", "ok",
                "running", "started", "stopped",
                "up to date", "synonyms=", "dynamic configs for topic"
            ],

            case lists:any(fun(M) -> string:find(Lower, M) =/= nomatch end, SuccessMarkers) of
                true ->
                    {ok, Output};
                false ->
                    case string:find(Lower, "error") of
                        nomatch ->
                            {ok, Output};
                        _ ->
                            {error, string:trim(Output)}
                    end
            end
    end;
execute(_Cmd) ->
    error.