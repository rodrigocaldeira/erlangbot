%%%-------------------------------------------------------------------
%% @doc erlangbot public API
%% @end
%%%-------------------------------------------------------------------

-module(erlangbot_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	User = os:getenv("twitch_user"),
	Password = os:getenv("twitch_password"),
	erlangbot_sup:start_link(User, Password).

stop(_State) ->
    ok.

%% internal functions
