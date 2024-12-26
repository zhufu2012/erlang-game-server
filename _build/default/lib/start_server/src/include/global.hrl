%%%-------------------------------------------------------------------
%%% @author xiexiaobo@d-game.cn
%%% @copyright (C) 2020, DoubleGame
%%% @doc
%%% 全局头文件
%%% @end
%%% Created : 2020-01-22 10:00
%%%-------------------------------------------------------------------
-author('xiexiaobo@d-game.cn').
-include("logger.hrl").

-ifndef(global_hrl).
-define(global_hrl, true).

-define(TRUE, true).
-define(FALSE, false).
-define(UNDEFINED, undefined).
-define(INFINITY, infinity).



-define(SECONDS_PER_MINUTE, 60).
-define(SECONDS_PER_HOUR, 3600).
-define(SECONDS_PER_DAY, 86400).
-define(SECONDS_PER_WEEK, 604800).

-define(MainStartDelayTick, 1000).  %%main开始延时
-define(MainTick_INTERVAL, 1000).  %%系统心跳包1000ms

-define(cache_time(Key, Time), time:cache_time(?FUNCTION_NAME, Key, Time)).
-define(get_cache_time(Key), time:get_cache_time(?FUNCTION_NAME, Key)).
-define(ModHandleMsg(Mod, Msg), {mod, Mod, Msg}).
-define(P_DIC_GET(Default), case get({?MODULE, ?FUNCTION_NAME}) of ?UNDEFINED -> Default; Value -> Value end).
-define(P_DIC_GET(Key, Default), case get({?MODULE, ?FUNCTION_NAME, Key}) of ?UNDEFINED -> Default; Value -> Value end).
-define(P_DIC_PUT(Value), put({?MODULE, ?FUNCTION_NAME}, Value)).
-define(P_DIC_PUT(Key, Value), put({?MODULE, ?FUNCTION_NAME, Key}, Value)).
-define(P_DIC_ERASE(Key), erase({?MODULE, Key})).
-define(P_DIC_ERASE(Key1, Key2), erase({?MODULE, Key1, Key2})).

-define(CALL(PID, Msg), gen_server:call(PID, Msg)).
-define(CALL(PID, Mod, Msg), gen_server:call(PID, {mod, Mod, Msg})).

-define(CAST(PID, Msg), gen_server:cast(PID, Msg)).
-define(CAST(PID, Mod, Msg), gen_server:cast(PID, {mod, Mod, Msg})).

-define(CAST_ROLE(RoleID, Msg), gen_server:cast(util:get_player_pid(RoleID), Msg)).
-define(CAST_ROLE(RoleID, Mod, Msg), gen_server:cast(util:get_player_pid(RoleID), {mod, Mod, Msg})).

-define(CAST_SELF(Msg), gen_server:cast(self(), Msg)).
-define(CAST_SELF(Mod, Msg), gen_server:cast(self(), {mod, Mod, Msg})).

-define(Now(), time:time()).
-define(Now2(), time:time_ms()).
-define(midnight(), time:midnight()).
-define(NowSec(), time:now_seconds()).

-define(DAY_SECONDS, 86400).
-define(HOUR_SECONDS, 3600).

-endif. %% -ifndef
