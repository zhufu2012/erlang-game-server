%%%-------------------------------------------------------------------
%%% @author zhufu
%%% @copyright (C) 2024, DoubleGame
%%% @doc
%%% 玩家管理进程
%%% @end
%%% Created : 10. 10月 2024 11:20
%%%-------------------------------------------------------------------
-module(player_manager).
-author("zhufu").
-behaviour(gen_server).

%% API
-export([start_link/0, cast_run/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_listener/0, stop_listener/0]).
-include("global.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).


%%%===================================================================
%%% API
%%%===================================================================

%% @hidden
start_link() ->
	Args = [],
	gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

cast(Request) ->
	gen_server:cast(?SERVER, Request).

%% @hidden Fun()
cast_run(Fun) when is_function(Fun, 0) ->
	cast({run, Fun}).

start_listener() ->
	Port = config:game_port(),
	tcp:start_listener(player_listener, Port, player_connection).
stop_listener() ->
	tcp:stop_listener(player_listener).
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
	ok.

-spec do_call(term(), {pid(), term()}) -> Reply :: term().
do_call(Request, From) ->
	?LOG_ERROR("no_match_call: Request=~p, From=~p", [Request, From]),
	ok.

-spec do_cast(term()) -> any().
do_cast({run, Fun}) ->
	Fun();
do_cast(Request) ->
	?LOG_ERROR("no_match_cast: Request=~p", [Request]).

-spec do_terminate(term()) -> any().
do_terminate(_Reason) ->
	ok.