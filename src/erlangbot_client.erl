-module(erlangbot_client).
-behaviour(gen_server).
-include("context.hrl").
-export([start/1, start_link/1, init/1, connect/0, send/1, stop/0, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2, handle_receive/1]).


start(Context) ->
	gen_server:start({local, ?MODULE}, ?MODULE, Context, []).

start_link(Context) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Context, []).

init(Context) ->
	{ok, Context}.

connect() ->
	gen_server:call(?MODULE, connect).

stop() ->
	gen_server:cast(?MODULE, stop).

handle_call(connect, _From, Context) ->
	case Context#context.connected of
		true -> 
			{reply, ok, Context};
		false ->
			Socket = connect(Context),
			NewContext = Context#context{socket=Socket, connected=true},
			{reply, ok, NewContext}
	end;

handle_call({send, Data}, _From, Context) ->
	send(Context#context.socket, Data),
	{reply, ok, Context}.

handle_cast(stop, Context) ->
	gen_tcp:close(Context#context.socket),
	{stop, normal, Context}.

handle_info({tcp_closed, _Socket}, Context) ->
	{noreply, Context};

handle_info(Message, Context) ->
	io:format("[TMI] ~p ~n", [Message]),
	{noreply, Context}.

code_change(_OldVersion, Context, _Extra) ->
	{ok, Context}.

terminate(_Reason, _Context) ->
	ok.

connect(Context) ->
	case gen_tcp:connect(Context#context.host,
			 Context#context.port,
			 [binary, {active, true}, {packet, line}, {keepalive, true}]) of
		{ok, Socket} ->
			send(Socket, "PASS " ++ Context#context.password),
			send(Socket, "NICK " ++ Context#context.user),
			Socket;
		{error, Reason} ->
			io:format("Error connecting: ~p ~n", [Reason]),
			error
	end.

send(Data) ->
	gen_server:call(?MODULE, {send, Data}).

send(Socket, Data) ->
	Message = Data ++ "\r\n",
	gen_tcp:send(Socket, Message).

handle_receive(Data) ->
	io:format("~p ~n", [Data]).
