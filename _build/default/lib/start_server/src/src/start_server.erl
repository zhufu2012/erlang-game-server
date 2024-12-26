%%%-------------------------------------------------------------------
%%% @author zhufu
%%% @copyright (C) 2024, DoubleGame
%%% @doc
%%% {{description}}
%%% @end
%%% Created : 09. 10月 2024 17:23
%%%-------------------------------------------------------------------
-module(start_server).
-author("zhufu").

-include("global.hrl").
-export([start/2, stop/1]).
-behaviour(application).
%%%===================================================================
%%% API
%%%===================================================================


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% Application callbacks


%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
	try
		start_before(),

		{ok, Pid} = gameserver_sup:start_link(),
		start_after(),

		{ok, Pid}
	catch
		Class:ExcReason:Stacktrace ->
			?LOG_ERROR(?LOG_STACKTRACE(Class, ExcReason, Stacktrace)),
			{error, ExcReason}
	end.

stop(_State) ->
	ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%进程树启动前
start_before() ->
	config:load_config(),
	0.

%%进程树启动后
start_after() ->
	gen_server:call(mainPID, {start}),
	%%player_manager:start_listener(),

	0.