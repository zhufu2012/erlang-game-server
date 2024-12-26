%%%-------------------------------------------------------------------
%%% @author zhufu
%%% @copyright (C) 2024, DoubleGame
%%% @doc
%%% {{description}}
%%% @end
%%% Created : 22. 4月 2024 11:15
%%%-------------------------------------------------------------------
-module(tcp_ce).
-author("zhufu").
-behaviour(gen_server).

-include("global.hrl").

%% API
-export([start_link/0, cast_run/1, cast_msg/1, call/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%%测试函数
-export([test/0]).

-define(SERVER, ?MODULE).

-record(state, {}).
-record(node_data, {
	key = 0,%%键
	type = 0,%%数据类型 1 数字 2 字符串 3 原子 4 元组 5列表
	value = 0 %%值
}).

-define(MaxConnectNum, 10).
-record(node_state, {
	node_name = java_node_name,
	node_s_name = 'java_node@127.0.0.1',
	state = ?FALSE,%%默认重启后是开启状态，开启状态才尝试连接外部节点，
	connect_num = 0,        %%尝试连接次数，每次连接失败加1 达到MaxConnectNum时停止连接
	list = [{is_print, ?FALSE}, {print_list, []}]            %%保存数据 [{key,value}]
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @hidden
start_link() ->
	Args = [],
	gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%% @hidden Fun()
cast_run(Fun) when is_function(Fun, 0) ->
	gen_server:cast(?SERVER, {run, Fun}).

cast_msg(Msg) ->
	case 1 of
		1 -> 1
	end,
	gen_server:cast(?SERVER, Msg).

call(Msg) ->
	gen_server:call(?SERVER, Msg).
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
		Reply = do_call(Request, From),
		{reply, Reply, State}
	catch
		Class:ExcReason:Stacktrace ->
			?LOG_ERROR(?LOG_STACKTRACE(Class, ExcReason, Stacktrace)),
			{reply, {error, ExcReason}, State}
	end.

%% @hidden
handle_cast(Request, State) ->
	try
		do_cast(Request)
	catch
		Class:ExcReason:Stacktrace ->
			?LOG_ERROR(?LOG_STACKTRACE(Class, ExcReason, Stacktrace))
	end,
	{noreply, State}.

%% @hidden
handle_info(Info, State) ->
	try
		do_cast(Info)
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
	erlang:send_after(1000, self(), tick),
	ok.

-spec do_call(term(), {pid(), term()}) -> Reply :: term().
do_call(is_print, _) ->
	?LOG_ERROR("~n~p", [1]);
do_call(Request, From) ->
	?LOG_ERROR("no_match_call: Request=~p, From=~p", [Request, From]),
	ok.

-spec do_cast(term()) -> any().
do_cast({run, Fun}) ->
	Fun();
do_cast(tick) ->
	erlang:send_after(1000, self(), tick);
do_cast(Request) ->
	?LOG_ERROR("no_match_cast: Request=~p", [Request]).

-spec do_terminate(term()) -> any().
do_terminate(_Reason) ->
	ok.

%%%====================================================================
%%% API functions
%%%===================================================================
test() ->
	0.


%%%====================================================================
%%% 外部命令
%%%===================================================================


%%%===================================================================
%%% Internal functions
%%%===================================================================

