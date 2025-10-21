%%%-------------------------------------------------------------------
%%% @author Alexander Todorov(alexander.todorov@ayagasha.com)
%%% @copyright (C) 2022, Ayagasha Enterprises Ltd.
%%% Company Ayagasha Enterprises Ltd.
%%% Created On: 14-Dec-2022
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_client).

-behaviour(gen_statem).
-include("../include/constants.hrl").
-export([start_link/3, init/1, callback_mode/0,  health_check/0, disconnect/0, connect/0]).
-export([handle_event/3]).
-export([connected/3, retry/3, disconnected/3]).


-record(client_state, {
    port = ?DEFAULT_PORT :: inet:port_number(),
    ip = ?DEFAULT_IP:: inet:ip_address(),
    monitor = false::boolean(),
    socket = undefined :: inet:socket(),
    status = disconnected :: connected | disconnected | retry,
    retried_at = 0 :: integer(),
    last_read_at = 0 :: integer()
}).
-spec(start_link(Host :: inet:ip_address(), Port :: inet:port_number(), Monitor :: boolean()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Host,Port, Monitor) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [Host,Port, Monitor], []).

-spec(init(Args :: list()) ->
  {ok, idle, State :: #client_state{}} | {ok, idle, State :: #client_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([IP, Port, Monitor]) ->
    
    case gen_tcp:connect(IP, Port, [binary, {packet, 0}]) of
      {ok, Socket} ->
          lager:info("Connected to server ~p is started on port: ~p", [IP, Port]),
          {ok, connected, #client_state{
            port = Port,
            ip = IP,
            monitor = Monitor,
            socket = Socket,
            status = connected
          }};
      {error, Reason} ->
        lager:info("Connection error: ~p", [Reason]),
        {ok, failed, 
          #client_state{
          port = Port,
          ip = IP,
          monitor = Monitor,
          status = disconnected
        }
        } 
    end.

callback_mode() -> state_functions.

-spec connected(EventType :: term(), EventContent :: term(), State :: #client_state{}) ->
  {next_state, StateName :: atom(), NewState :: #client_state{}} |
  {next_state, StateName :: atom(), NewState :: #client_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #client_state{}} |
  {keep_state, NewState :: #client_state{}} |
  {keep_state, NewState :: #client_state{}, timeout() | hibernate}.
connected(_, {tcp, _Socket, Message}, State) ->
  Now = erlang:system_time(millisecond),
  lager:info("#received: ~p at: ~p", [Message, Now]),
    {keep_state, State#client_state{last_read_at = Now}};

connected(_, {tcp_closed, _Socket, _Message}, State = #client_state{port=Port, ip = IP}) ->
  lager:info("disconnected from : [~p:~p]", [IP, Port]),
    {next_state, disconnected, State#client_state{socket = undefined, status=disconnected}};

connected({call, From}, disconnect, State = #client_state{port=Port, ip = IP, socket = Socket}) ->
  lager:info("disconnecting from : [~p:~p]", [IP, Port]),
    gen_tcp:close(Socket),  
    {next_state, disconnected, State#client_state{socket = undefined, status=disconnected},
      [{reply, From, disconnected}]};  

connected(Event, Type, State) ->
  lager:info("current state: connected"),
  lager:info("Event: ~p, Type: ~p, State: ~p", [Event, Type, State]),
  handle_event(Event, Type, State).

retry(_, connect, State = #client_state{retried_at = Time}) ->
  Now = erlang:system_time(millisecond),
  if
    Now - Time > ?READ_TIMEOUT_MS ->
        lager:info("reconnecting to server"),
        case gen_tcp:connect(State#client_state.ip, State#client_state.port, [binary, {packet, 0}]) of  
            {ok, Socket} ->
                lager:info("Connected to server ~p is started on port: ~p",
                           [State#client_state.ip, State#client_state.port]),
                {next_state, connected, State#client_state{socket = Socket, status = connected}};
            {error, Reason} ->
                lager:info("Connection error: ~p", [Reason]),
                {keep_state, State#client_state{retried_at = Now, status = disconnected}}
        end;
    true ->
        lager:error("reconnect timeout should be ~p ms", [?READ_TIMEOUT_MS]),
        {keep_state, State}
  end;

retry(_, _, State) ->
  {keep_state, State}.

disconnected({call, From}, connect, State) ->
  lager:info("reconnecting to server"),
  case gen_tcp:connect(State#client_state.ip, State#client_state.port, [binary, {packet, 0}]) of  
    {ok, Socket} ->
      lager:info("Connected to server ~p is started on port: ~p", [State#client_state.ip, State#client_state.port]),
      {next_state, connected, State#client_state{socket = Socket, status = connected}, [{reply,From, ok}]};
    {error, Reason} ->
      lager:info("Connection error: ~p", [Reason]),
      {next_state, retry, State#client_state{retried_at = erlang:system_time(millisecond)}, [{reply,From, failure}]}
  end;
disconnected(Event, Type, State) ->
  handle_event(Event, Type, State).

handle_event({call, From}, health_check, State = #client_state{status = disconnected}) ->
    lager:info("disconnected"),
    {next_state, disconnected, State, [{reply,From, disconnected}]};

handle_event({call, From}, health_check, State = #client_state{last_read_at=LastReadAt, status = connected}) ->
  
  Now = erlang:system_time(millisecond),
  lager:info("health check. Now: ~p, LastReadAt: ~p", [Now, LastReadAt]),
  if Now - LastReadAt > ?CONNECTION_TIMEOUT_MS ->
    lager:info("timeout. last read at ~p", [LastReadAt]),
    {keep_state, State, [{reply,From, timeout}]};
  true ->
    lager:info("Keep alive"),
    {keep_state, State, [{reply,From, up_and_running}]}
  end;

handle_event(_, _, State) ->
  lager:error("unknown event"),
  {keep_state, State}.



health_check() ->
  gen_statem:call(?MODULE, health_check).

disconnect() ->
  gen_statem:call(?MODULE, disconnect).

connect() ->
  gen_statem:call(?MODULE, connect).