%%%-------------------------------------------------------------------
%%% @author Alexander Todorov(alexander.todorov@ayagasha.com)
%%% @copyright (C) 2022, Ayagasha Enterprises Ltd.
%%% Company Ayagasha Enterprises Ltd.
%%% Created On: 16-Dec-2022
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_server).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {socket}).

-spec start_link(Socket :: inet:socket()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Socket) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Socket, []).

-spec init(Socket :: inet:socket()) -> {ok, State :: #state{}}.
init(Socket) ->
   gen_server:cast(?MODULE, accept),
  {ok, #state{socket=Socket}}.

-spec handle_cast(accept, State :: #state{}) -> {noreply, State :: #state{}}.
handle_cast(accept, State = #state{socket=ListenSocket}) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  ok = gen_tcp:send(AcceptSocket, "Connection established"),
  {noreply, State#state{socket=AcceptSocket}};
handle_cast(_, State) ->
  {noreply, State}.

-spec handle_call(Bin :: binary(), From :: {pid(), Tag :: term()}, State :: #state{}) -> {reply, ok, State :: #state{}}.
handle_call(Bin, _From, State=#state{socket=Socket}) -> 
  case gen_tcp:send(Socket, Bin) of
    ok -> {reply, ok, State};
    {error, Reason} -> 
        {reply, failed, State},
        lager:info("Failed to send message: ~p", [Reason])
  end;

handle_call(_E, _From, State) -> {noreply, State}.

-spec handle_info({tcp, Socket :: inet:socket(), Msg :: binary()}, State :: #state{}) -> {noreply, State :: #state{}}.
handle_info({tcp, Socket, Msg}, State) ->
  gen_tcp:send(Socket, Msg),
  {noreply, State};
handle_info({tcp_closed, _Socket}, State) -> {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) -> {stop, normal, State};
handle_info(E, State) ->
  lager:info("unsupported: ~p~n", [E]),
  {noreply, State}.


terminate(_Reason, _Tab) -> ok.
code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.

