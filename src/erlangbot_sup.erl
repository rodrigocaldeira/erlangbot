%%%-------------------------------------------------------------------
%% @doc erlangbot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlangbot_sup).

-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(User, Password) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [User, Password]).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([User, Password]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 1},
    ChildSpecs = [#{
	id => erlangbot_server,
	start => {erlangbot_server, start, [User, Password]},
	restart => permanent,
	type => worker
     }],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
