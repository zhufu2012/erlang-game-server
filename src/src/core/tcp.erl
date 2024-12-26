%%%-------------------------------------------------------------------
%%% @author xiexiaobo@d-game.cn
%%% @copyright (C) 2020, DoubleGame
%%% @doc
%%% TCP接口
%%% @end
%%% Created : 2020-01-22 10:00
%%%-------------------------------------------------------------------
-module(tcp).
-author('xiexiaobo@d-game.cn').

%% API
-export([start_listener/3, start_listener/6, stop_listener/1]).
-export([start_connector/4, start_connector/6, stop_connector/1]).
-export([cast/2]).
-export([send/2]).
-export([close/2]).
-export([start_login_timer/0, cancel_login_timer/0]).

-include("global.hrl").

-define(INET, inet).
-define(PACKET_RAW, raw).
-define(KEEPALIVE_INTERVAL, 60 * 1000).
-define(NUM_ACCEPTORS, 100).
-define(LOGIN_TIMEOUT, 30 * 1000).


%%%===================================================================
%%% d_tcp_protocol callbacks
%%%===================================================================

%% 进程消息
-callback handle_cast(Request :: term()) -> any().

%% 连接成功
-callback handle_connect(PeerAddress :: string()) -> any().

%% 消息编码
-callback handle_encode(MsgList :: [term()]) -> {ok, DataList :: [iodata()]} | {error, Reason :: term()}.

%% 消息解码
-callback handle_decode(State :: term(), Buffer :: binary()) -> {ok, NewState :: term(), NewBuffer :: binary()} | {error, Reason :: term()}.

%% 保持连接
-callback handle_keepalive(Interval :: pos_integer()) -> any().

%% 连接关闭
-callback handle_close(Reason :: term()) -> any().


%%%===================================================================
%%% API
%%%===================================================================

start_listener(Name, Port, Handler) ->
	start_listener(Name, Port, Handler, ?PACKET_RAW, ?KEEPALIVE_INTERVAL, ?NUM_ACCEPTORS).
start_listener(Name, Port, Handler, Packet, KeepaliveInterval, NumAcceptors) when is_atom(Name), is_atom(Handler) ->
	d_tcp_server:start(Name, Port, Handler, ?INET, Packet, KeepaliveInterval, NumAcceptors).
%%
stop_listener(Name) when is_atom(Name) ->
	d_tcp_server:stop(Name).

%% 返回 {ok, ConnPid} | {error, Reason}
start_connector(Name, PeerAddress, PeerPort, Handler) ->
	start_connector(Name, PeerAddress, PeerPort, Handler, ?PACKET_RAW, ?KEEPALIVE_INTERVAL).
start_connector(Name, PeerAddress, PeerPort, Handler, Packet, KeepaliveInterval) when is_atom(Name), is_atom(Handler) ->
	d_tcp_client:start(Name, PeerAddress, PeerPort, Handler, ?INET, Packet, KeepaliveInterval).
%%
stop_connector(NameOrConnPid) ->
	d_tcp_client:stop(NameOrConnPid).

%% 进程消息
cast(ConnPid, Request) when is_pid(ConnPid) ->
	gen_server:cast(ConnPid, Request).

%% 发送消息
send(?UNDEFINED, _MsgOrMsgList) -> ok;
send(ConnPid, MsgList) when is_list(MsgList) ->
	do_send(ConnPid, MsgList);
send(ConnPid, Msg) ->
	do_send(ConnPid, [Msg]).

%% 关闭连接
close(?UNDEFINED, _Reason) -> ok;
close(ConnPid, Reason) ->
	do_close(ConnPid, Reason).

%% 登录超时, 5秒超时
start_login_timer() ->
	d_tcp_protocol:start_login_timer(?LOGIN_TIMEOUT).
cancel_login_timer() ->
	d_tcp_protocol:cancel_login_timer().


%%%===================================================================
%%% Internal functions
%%%===================================================================

do_send(_ConnPid, []) -> ok;
do_send(ConnPid, MsgList) when ConnPid =:= self() ->
	d_tcp_protocol:add_action({tcp_send, MsgList});
do_send(ConnPid, MsgList) when is_pid(ConnPid) ->
	cast(ConnPid, {tcp_send, MsgList}).

do_close(ConnPid, Reason) when ConnPid =:= self() ->
	d_tcp_protocol:add_action({tcp_close, Reason});
do_close(ConnPid, Reason) when is_pid(ConnPid) ->
	cast(ConnPid, {tcp_close, Reason}).
