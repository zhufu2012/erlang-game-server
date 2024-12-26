%%%-------------------------------------------------------------------
%%% @author zhufu
%%% @copyright (C) 2024, DoubleGame
%%% @doc
%%%  进程间共享的数据
%%% @end
%%% Created : 09. 10月 2024 20:20
%%%-------------------------------------------------------------------
-module(data).
-author("zhufu").

%% API
-export([storage_put/2, storage_get/1, storage_get/2]).

-include("global.hrl").


%%%===================================================================
%%% API
%%%===================================================================
%% 进程共享的KeyValue内存存储
storage_put(Key, Value) -> d_storage:put(Key, Value).
storage_get(Key) -> d_storage:get(Key).
storage_get(Key, Default) -> d_storage:get(Key, Default).


%%进程共享键 存储字典数据
storage_put_dict_key(DictKey, Key, Value) ->
	Dict = storage_get(DictKey, dict:new()),
	storage_put(DictKey, dict:append(Key, Value, Dict)).
%% 获取字典中某个键的值
storage_get_dict_key(DictKey, Key, Default) ->
	Dict = storage_get(DictKey, dict:new()),
	case dict:find(Key, Dict) of
		{ok, V} -> V;
		_ -> Default
	end.
%%获取字典值
storage_get_dict(DictKey, Default)->
	d_storage:get(DictKey, Default).


%%%===================================================================
%%% Internal functions
%%%===================================================================