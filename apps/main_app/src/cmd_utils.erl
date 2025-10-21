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
    case cmd_utils:os_cmd_with_timeout(Cmd, 5000) of
        {error, timeout} ->
            {error, "command timeout"};

        Output when is_list(Output) ->
            Lower = string:lowercase(Output),
            case string:find(Lower, ?ERROR) of
                nomatch ->
                    {ok, Output};
                _ ->
                    TrimmedOutput = string:trim(Output),
                    case unicode:characters_to_list(TrimmedOutput) of
                        {error, _, _} ->
                            case unicode:characters_to_list(TrimmedOutput, latin1) of
                                Result when is_list(Result) ->
                                    {error, Result};
                                _ ->
                                    {error, "unicode conversion failed"}
                            end;
                        {incomplete, _, _} ->
                            case unicode:characters_to_list(TrimmedOutput, latin1) of
                                Result when is_list(Result) ->
                                    {error, Result};
                                _ ->
                                    {error, "unicode conversion failed"}
                            end;
                        Result ->
                            {error, Result}
                    end
            end
    end;
execute(_Cmd) ->
    error.   
