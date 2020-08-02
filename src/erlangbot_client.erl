-module(erlangbot_client).
-behaviour(gen_server).
-include("context.hrl").
-export([start/1, start_link/1, init/1, connect/0, send/1, stop/0, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).


start(Context) ->
	gen_server:start({local, ?MODULE}, ?MODULE, Context, []).

start_link(Context) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Context, []).

init(Context) ->
	{ok, Context}.

connect() ->
	gen_server:call(?MODULE, connect),
	login().

stop() ->
	gen_server:cast(?MODULE, stop).

login() ->
	gen_server:call(?MODULE, login).

handle_call(connect, _From, Context) ->
	case Context#context.connected of
		true -> 
			{reply, ok, Context};
		false ->
			case connect(Context) of
				Socket ->
					io:format("Conectado com sucesso. ~n"),
					NewContext = Context#context{socket=Socket, connected=true},
					{reply, ok, NewContext};
				error ->
					{reply, error, Context}
			end
	end;

handle_call(login, _From, Context) ->
	io:format("Login... ~n"),
	ssl:send(Context#context.socket, "PASS " ++ Context#context.password ++ "\r\n"),
	ssl:send(Context#context.socket, "NICK " ++ Context#context.user ++ "\r\n"),
	{reply, ok, Context};

handle_call({send, Data}, _From, Context) ->
	ssl:send(Context#context.socket, Data ++ "\r\n"),
	{reply, ok, Context}.

handle_cast(stop, Context) ->
	ssl:close(Context#context.socket),
	{stop, normal, Context}.

handle_info({ssl_closed, _Socket}, Context) ->
	{noreply, Context};

handle_info({ssl, _, <<"PING", _/binary>>}, Context) ->
	io:format("PING PONG ~n"),
	ssl:send(Context#context.socket, "PONG :tmi.twitch.tv\r\n"),
	{noreply, Context};

handle_info({ssl, _, Mensagem}, Context) ->
	io:format("~p ~n", [Mensagem]),
	{noreply, Context};

handle_info(Mensagem, Context) ->
	io:format("[MENSAGEM DESCONHECIDA] ~p ~n", [Mensagem]),
	{noreply, Context}.

code_change(_OldVersion, Context, _Extra) ->
	{ok, Context}.

terminate(_Reason, _Context) ->
	ok.

connect(Context) ->
	ssl:start(),
	case ssl:connect(Context#context.host,
			 Context#context.port,
			 [binary, {active, true}, {packet, line}, {keepalive, true}]) of
		{ok, Socket} ->
			Socket;
		{error, Reason} ->
			io:format("Erro ao conectar: ~p ~n", [Reason]),
			error
	end.

send(Data) ->
	gen_server:call(?MODULE, {send, Data}).

