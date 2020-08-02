-module(erlangbot_server).
-behaviour(gen_server).
-export([start/2, init/1, connect/0, disconnect/0, handle_call/3, handle_cast/2, join/1, part/1, send/1]).
-include("context.hrl").

start(User, Password) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, {"irc.chat.twitch.tv", 6667, User, Password}, []).


init({Host, Port, User, Password}) ->
	Context = #context{host=Host,
		       port=Port,
		       password=Password,
		       user=User},
	case whereis(erlangbot_client) of
		undefined ->
			erlangbot_client:start(Context);
		_Pid ->
			ok
	end,
	{ok, Context}.

connect() ->
	gen_server:call(?MODULE, connect).

disconnect() ->
	gen_server:cast(?MODULE, disconnect).

send(Data) ->
	gen_server:call(?MODULE, {send, Data}).

join(Channel) ->
	gen_server:call(?MODULE, {join, Channel}).

part(Channel) ->
	gen_server:call(?MODULE, {part, Channel}).

handle_call(connect, _From, Context) ->
	erlangbot_client:connect(),
	{reply, ok, Context};

handle_call({send, Data}, _From, Context) ->
	erlangbot_client:send(Data),
	{reply, ok, Context};

handle_call({join, Channel}, _From, Context) ->
	erlangbot_client:send("JOIN " ++ Channel),
	{reply, ok, Context};

handle_call({part, Channel}, _From, Context) ->
	erlangbot_client:send("PART " ++ Channel),
	{reply, ok, Context}.

handle_cast(disconnect, Context) ->
	erlangbot_client:stop(),
	{stop, normal, Context}.
