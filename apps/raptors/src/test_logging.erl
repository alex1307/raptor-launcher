-module(test_logging).
-export([demo/0]).

%% Демонстрация на новия лог формат с модул:функция:линия

demo() ->
    io:format("~n=== Testing Lager Logging Format ===~n~n"),
    
    %% Тестови логове на различни нива
    lager:debug("This is a debug message from test_logging"),
    lager:info("This is an info message with details: ~p", [#{key => value}]),
    lager:warning("This is a warning - something might be wrong"),
    lager:error("This is an error message with reason: ~p", [{error, test_error}]),
    
    io:format("~nCheck the logs - you should see:~n"),
    io:format("  YYYY-MM-DD HH:MM:SS [DEBUG] test_logging:demo:11 This is a debug message...~n"),
    io:format("  YYYY-MM-DD HH:MM:SS [INFO] test_logging:demo:12 This is an info message...~n"),
    io:format("  YYYY-MM-DD HH:MM:SS [WARNING] test_logging:demo:13 This is a warning...~n"),
    io:format("  YYYY-MM-DD HH:MM:SS [ERROR] test_logging:demo:14 This is an error message...~n~n"),
    
    ok.
