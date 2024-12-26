%%%-------------------------------------------------------------------
%%% @author zhufu
%%% @copyright (C) 2024, DoubleGame
%%% @doc
%%% {{description}}
%%% @end
%%% Created : 09. 10月 2024 19:43
%%%-------------------------------------------------------------------
-module(main).
-author("zhufu").
-behaviour(gen_server).

-include("global.hrl").
-include("ets_name.hrl").
-include("globalDict.hrl").

%% API
-export([start_link/0, cast_run/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([set_login_enable/1, get_login_enable/0]).

-define(SERVER, ?MODULE).

-record(state, {}).


%%%===================================================================
%%% API
%%%===================================================================

%% @hidden
start_link() ->
	Args = [],
	gen_server:start_link({local, mainPID}, ?MODULE, [], [{timeout, 120000}]).

%% @hidden Fun()
cast_run(Fun) when is_function(Fun, 0) ->
	gen_server:cast(?SERVER, {run, Fun}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @hidden
init(Args) ->
	try
		do_init(Args),
		{ok, #state{}}
	catch
		Class:ExcReason:Stacktrace ->
			?LOG_ERROR(?LOG_STACKTRACE(Class, ExcReason, Stacktrace)),
			{stop, ExcReason}
	end.

%% @hidden
handle_call(Request, From, State) ->
	try
		Reply = do_call(Request, From, State),
		{reply, Reply, State}
	catch
		Class:ExcReason:Stacktrace ->
			?LOG_ERROR(?LOG_STACKTRACE(Class, ExcReason, Stacktrace)),
			{reply, {error, ExcReason}, State}
	end.

%% @hidden
handle_cast(Request, State) ->
	try
		do_cast(Request, State)
	catch
		Class:ExcReason:Stacktrace ->
			?LOG_ERROR(?LOG_STACKTRACE(Class, ExcReason, Stacktrace))
	end,
	{noreply, State}.

%% @hidden
handle_info(Info, State) ->
	try
		do_cast(Info, State)
	catch
		Class:ExcReason:Stacktrace ->
			?LOG_ERROR(?LOG_STACKTRACE(Class, ExcReason, Stacktrace))
	end,
	{noreply, State}.

%% @hidden
terminate(Reason, _State) ->
	try
		do_terminate(Reason)
	catch
		Class:ExcReason:Stacktrace ->
			?LOG_ERROR(?LOG_STACKTRACE(Class, ExcReason, Stacktrace))
	end,
	ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec do_init(term()) -> any().
do_init(_Args) ->
	createEtsTable(),%%创建全局ets表
	ok.



do_call({start}, _, _) ->%%main开始运行
	main_start(),
	erlang:send_after(?MainStartDelayTick, self(), {tick}),
	ok;

do_call({stop}, _, _) ->%%main结束运行

	ok;
do_call(Request, From, _) ->
	?LOG_ERROR("no_match_call: Request=~p, From=~p", [Request, From]),
	ok.


do_cast({tick}, _State) ->
	on_Tick(),
	erlang:send_after(?MainTick_INTERVAL, self(), {tick}),
	ok;
do_cast({run, Fun}, _State) ->
	Fun();
do_cast(Request, State) ->
	?LOG_ERROR("no_match_cast: Request=~p,State=~p", [Request, State]).

-spec do_terminate(term()) -> any().
do_terminate(_Reason) ->
	ok.




%%创建全局ets表
createEtsTable() ->
	ets:new(?Ets_Global_InitSocket, [{keypos, #globalSocket.socket}, named_table, public, set, ?ETSRC, ?ETSWC]),%%等待通过验证
	ets:new(?ETS_Global_PlayerOnLine, [{keypos, #playerOnLine.player_id}, named_table, public, set, ?ETSRC, ?ETSWC]),%%角色在线信息
	ets:new(?ETS_Global_WorldEvent, [{keypos, #worldEvent.id}, named_table, public, set, ?ETSRC, ?ETSWC]),%% 全服离线事件

	ok.

%%
main_start() ->
	0.



%%
on_Tick() ->
	NowSecond = time:time(),%%当前秒数
	NowMinute = NowSecond div 60,%%当前分钟数
	case get(?LastTickSeconds) =/= NowSecond of
		?TRUE ->%%每秒运行

			ok;
		_ -> ok
	end,

	case get(?LastTickMinute) =/= NowMinute of
		?TRUE ->%%每分钟运行
			ok;
		_ -> ok
	end.




%% 设置登录开关
set_login_enable(IsEnable) ->
	data:storage_put(main_login_enable, IsEnable).
%%获取登录开关
get_login_enable() ->
	data:storage_get(main_login_enable, ?FALSE).





