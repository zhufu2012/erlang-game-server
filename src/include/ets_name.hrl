%%%-------------------------------------------------------------------
%%% @author zhufu
%%% @copyright (C) 2024, DoubleGame
%%% @doc
%%% Ets表名称
%%% @end
%%% Created : 09. 10月 2024 19:46
%%%-------------------------------------------------------------------
-author("zhufu").
-include("global.hrl").

-ifndef(ets_name_hrl).
-define(ets_name_hrl, true).

-define(ETSRC, {read_concurrency, true}).       %%并发读写
-define(ETSWC, {write_concurrency, true}).      %%并发读写


-define(Ets_Global_InitSocket, ets_InitSocket).%%等待通过验证
-record(globalSocket, {
	socket, pid, time,
	state = 0,            %% 0-初始状态、1-排队状态、2-登录忙碌状态、3-登录空闲状态
	queue_time = 0        %% 排队时间
}).


-define(ETS_Global_PlayerOnLine, ets_playerOnLine). %%玩家在线公共信息
-record(playerOnLine, {
	player_id = 0,        		%%玩家id
	pid = 0,                	%%玩家 进程preocess id
	is_connected = ?TRUE,    	%%是否有网络连接
	name = ""                	%%玩家名称
}).


-define(ETS_Global_WorldEvent, worldEventEts).  %% 全服离线事件
-record(worldEvent, {
	id = 0,
	type = 0,       %% 类型
	time = 0,       %% 开始时间
	msg = {}        %% 内容
}).



-endif. %% -ifndef
