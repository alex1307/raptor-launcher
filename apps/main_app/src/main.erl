%%%-------------------------------------------------------------------
%%% @author Alexander Todorov(alexander.todorov@ayagasha.com)
%%% @copyright (C) 2022, Ayagasha Enterprises Ltd.
%%% Company Ayagasha Enterprises Ltd.
%%% Created On: 14-Dec-2022
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(main).
-author("Alexander Todorov(alexander.todorov@ayagasha.com)").

-behaviour(application).

%% Application callbacks
-export([
  start/2,
  stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  lager:info("Starting main app"),
  % docker_srv:start_link(),
  % timer:sleep(2_000),
  % {ok, Status} = docker_srv:status(),
  % lager:info("Docker status: ~p", [Status]),
  % {ok, Pid} = service_manager_srv:start_link(),
  % lager:info("service manager has been started successfully. Pid = ~p", [Pid]),
  % Res = gen_server:call(service_manager_srv, {docker, status}),
  % lager:info("Response: ~p", [Res]),
  % Exists = kafka_utils:describe_topic("raptor.mobile_de"),
  % lager:info("Topic exists: ~p", [Exists]),
  % {ok, Config} = gen_server:call(service_manager_srv, {describe_topic, "raptor.mobile_de"}),
  % lager:info("Topic configuration: ~p", [Config]),
  % {ok, Pid} = app_supervisor:start_link(),
  % lager:info("Supervisor manager has been started successfully. Pid = ~p", [Pid]),
  % {ok, ListenSocket} = gen_tcp:listen(?DEFAULT_PORT, [{active,true}]),
  % app_supervisor:start_child(server_sup, tcp_server, start_link, [ListenSocket]),
  % app_supervisor:start_child(client_sup, tcp_client, start_link, [?DEFAULT_IP, ?DEFAULT_PORT, true]),
  % lager:info("=== STARTED: SUCCESS ==="),
  % spawn(fun() -> client_test() end),
  {ok, self()}.

% client_test() ->
%   lager:info("=== CLIENT TEST ==="),
%   timer:sleep(1_000),
%   up_and_running = tcp_client:health_check(),
%   lager:info("=== 0 ==="),
%   timer:sleep(?READ_TIMEOUT_MS),
%   timeout = tcp_client:health_check(),
%   lager:info("=== 1 ==="),
%   disconnected = tcp_client:disconnect(),
%   lager:info("=== 2 ==="),
%   disconnected = tcp_client:health_check(),
%   lager:info("=== 3 ==="), 
%   tcp_client:connect(),
%   lager:info("=== 4 ==="),
%   timer:sleep(1_000),
%   lager:info("=== 5 ==="),
%   up_and_running = tcp_client:health_check(),
%   gen_server:call(tcp_server, "xxxx"),
%   lager:info("Test completed successfully"),
%   application:stop(main).






%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
