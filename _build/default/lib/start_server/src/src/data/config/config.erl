%%%-------------------------------------------------------------------
%%% @author zhufu
%%% @copyright (C) 2024, DoubleGame
%%% @doc
%%% {{description}}
%%% @end
%%% Created : 10. 10月 2024 11:23
%%%-------------------------------------------------------------------
-module(config).
-author("zhufu").

%% API
-compile(export_all).

-include("global.hrl").


-define(PUT(Value), persistent_term:put({?MODULE, ?FUNCTION_NAME}, Value)).
-define(GET(), persistent_term:get({?MODULE, ?FUNCTION_NAME})).


-define(C2L(B), unicode:characters_to_list(B)).

%%%===================================================================
%%% API
%%%===================================================================
%%加载配置数据
load_config() ->
	{ok, Dir} = file:get_cwd(),
	[RootDir | _] = string:split(Dir, "_build"),
	root_dir(RootDir),%%保存根目录数据
	Filename = filename:join([RootDir, "config.txt"]),
	load_database(Filename).

load_database(Filename) ->
	{ok, ConfigList} = d_conf:parse_file(Filename),
	{ok, ServerId} = get_integer("server_id", ConfigList),
	{ok, ServerName} = get_string("server_name", ConfigList),
	{ok, GamePort} = get_string("game_port", ConfigList),
	server_id(ServerId),
	server_name(?C2L(ServerName)),
	game_port(GamePort),
	0.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% 返回 {ok, Value} | error
get_string(Key, KeyValueList) ->
	case proplists:get_value(Key, KeyValueList) of
		?UNDEFINED ->
			error;
		Value ->
			{ok, Value}
	end.
get_integer(Key, KeyValueList) ->
	case get_string(Key, KeyValueList) of
		error ->
			error;
		{ok, Value} ->
			{ok, list_to_integer(Value)}
	end.

root_dir(Value) -> ?PUT(Value).
root_dir() -> ?GET().

server_id(Value) -> ?PUT(Value).
server_id() -> ?GET().

server_name(Value) -> ?PUT(Value).
server_name() -> ?GET().


game_port(Value) -> ?PUT(Value).
game_port() -> ?GET().
