%%%-------------------------------------------------------------------
%%% @author zhufu
%%% @copyright (C) 2024, DoubleGame
%%% @doc
%%% {{description}}
%%% @end
%%% Created : 11. 10月 2024 9:45
%%%-------------------------------------------------------------------
-module(player_connection).
-author("zhufu").
-behaviour(tcp).
-include("global.hrl").

%% API
-export([start_link/0, cast_run/1]).
%% tcp callbacks
-export([handle_cast/1, handle_connect/1, handle_encode/1, handle_decode/2, handle_keepalive/1, handle_close/1]).

-define(SERVER, ?MODULE).

-record(state, {
	peer_address,
	decrypt_tuple,
	player_id,
	account,
	player_pid
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


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%进程消息
handle_cast(_Reason) ->
	ok.

%%连接成功
handle_connect(PeerAddress) ->
	?LOG_NOTICE("PeerAddress=~s", [PeerAddress]),

	ok.

%%消息编码
handle_encode(MsgList) ->
	ok.
%%消息解码
handle_decode(State, Buffer) ->
	ok.

%% 保持连接
handle_keepalive(_Interval) ->
	ok.

%% 连接关闭
handle_close(Reason) ->
	ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
