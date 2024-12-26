%%%-------------------------------------------------------------------
%%% @author zhufu
%%% @copyright (C) 2024, DoubleGame
%%% @doc
%%% 进程树
%%% @end
%%% Created : 09. 10月 2024 17:28
%%%-------------------------------------------------------------------
-module(gameserver_sup).
-author("zhufu").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%-include("global.hrl").

-define(SERVER, ?MODULE).
-define(CHILD_WORKER(Module),
	#{id => Module, start => {Module, start_link, []}, restart => temporary, shutdown => 5000, type => worker, modules => [Module]}).
-define(CHILD_SUPERVISOR(Module),
	#{id => Module, start => {Module, start_link, []}, restart => temporary, shutdown => infinity, type => supervisor, modules => [Module]}).


start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).



init([]) ->
	SupFlags = #{strategy => one_for_all, intensity => 0, period => 1},
	ChildSpecs = [
		?CHILD_WORKER(main),


		?CHILD_WORKER(tcp_ce)
	],
	{ok, {SupFlags, ChildSpecs}}.