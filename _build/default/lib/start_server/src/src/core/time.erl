%%%-------------------------------------------------------------------
%%% @author xiexiaobo@d-game.cn
%%% @copyright (C) 2020, DoubleGame
%%% @doc
%%% 时间函数
%%% @end
%%% Created : 2020-01-22 10:00
%%%-------------------------------------------------------------------
-module(time).
-author('xiexiaobo@d-game.cn').

%% API
-export([set_offset/1, get_offset/0]).
-export([time/0, time_ms/0]).
-export([time_to_localtime/1, localtime_to_time/1]).
-export([seconds_of_week/1]).
-export([day_of_week/1, day_of_week/0]).
-export([week_number/1, week_number/0]).
-export([localtime_add/2, localtime_sub/2, localtime_offset/2]).
-export([is_dst/1, is_dst/0, timezone_offset/1, timezone_offset/0]).
-export([format/1, format_date/1, format_time/1]).
-export([format_rfc3339/1, format_timezone/1]).
%%
-export([set_localtime/1, get_localtime/0]).
-export([time_add/2, time_sub/2, time_offset/2]).
-export([daytime/1, daytime/0, daytime_add/2, daytime_offset/1, daytime_offset_days/2, daytime_sub_days/2]).
-export([daytime5/1, daytime5/0, daytime5_add/2, daytime5_offset/1, daytime5_offset_days/2, daytime5_sub_days/2]).
-export([weektime/1, weektime/0, weektime_add/2, weektime_offset/1]).
-export([weektime5/1, weektime5/0, weektime5_add/2, weektime5_offset/1]).
-export([cache_time/3, get_cache_time/2]).
-export([next_sunday_end_time/0, day_start_second/0, day_start_second/1, is_same_day/2, is_same_week/2]).
-export([
	timestamp_to_datetime/1,
	datetime_to_timestamp/1,
	day_diff/2,
	midnight/0,
	midnight/1,
	now_seconds/0
]).

-include("global.hrl").

-define(MIN_TIME, 1577808000).        %% {{2020,1,1},{0,0,0}}


%%%===================================================================
%%% API
%%%===================================================================

%% 设置Unix时间戳偏移秒数
-spec set_offset(integer()) -> ok.
set_offset(Seconds) ->
	d_time:set_offset(Seconds).

%% 获取Unix时间戳偏移秒数
-spec get_offset() -> Seconds :: integer().
get_offset() ->
	d_time:get_offset().

%% Unix时间戳（秒）
-spec time() -> Time :: pos_integer().
time() ->
	d_time:time().

%% Unix时间戳（毫秒）
-spec time_ms() -> TimeMS :: pos_integer().
time_ms() ->
	d_time:time_ms().

%% Unix时间戳（秒）-> 本地时间
-spec time_to_localtime(non_neg_integer()) -> LocalTime :: calendar:datetime().
time_to_localtime(Time) ->
	d_time:time_to_localtime(Time).

%% 本地时间 -> Unix时间戳（秒）
-spec localtime_to_time(calendar:datetime()) -> Time :: non_neg_integer().
localtime_to_time(LocalTime) ->
	d_time:localtime_to_time(LocalTime).

%% 本周秒数
-spec seconds_of_week(calendar:date()) -> Seconds :: integer().
seconds_of_week(Date) ->
	d_time:seconds_of_week(Date).

%% 周几，1..7依次表示周一到周日
-spec day_of_week(LocalTime :: calendar:datetime() | (Date :: calendar:date()) | (Time :: non_neg_integer())) -> DayNum :: calendar:daynum().
day_of_week(LocalTimeOrDateOrTime) ->
	d_time:day_of_week(LocalTimeOrDateOrTime).
day_of_week() ->
	day_of_week(?MODULE:time()).

%% 周数，代表不同周的递增数字
-spec week_number(LocalTime :: calendar:datetime() | (Date :: calendar:date()) | (Time :: non_neg_integer())) -> WeekNum :: non_neg_integer().
week_number(LocalTimeOrDateOrTime) ->
	d_time:week_number(LocalTimeOrDateOrTime).
week_number() ->
	week_number(?MODULE:time()).

%% 本地时间增减秒数
-spec localtime_add(calendar:datetime(), integer()) -> LocalTime :: calendar:datetime().
localtime_add(LocalTime, Seconds) ->
	d_time:localtime_add(LocalTime, Seconds).

%% 本地时间相差秒数
-spec localtime_offset(calendar:datetime(), calendar:datetime()) -> Seconds :: integer().
localtime_offset(LocalTimeFrom, LocalTimeTo) ->
	d_time:localtime_offset(LocalTimeFrom, LocalTimeTo).
localtime_sub(LocalTimeTo, LocalTimeFrom) ->
	localtime_offset(LocalTimeFrom, LocalTimeTo).

%% 是否是夏令时
-spec is_dst(LocalTime :: calendar:datetime() | (Time :: non_neg_integer())) -> boolean().
is_dst(LocalTimeOrTime) ->
	d_timezone:is_dst(LocalTimeOrTime).
is_dst() ->
	d_timezone:is_dst().

%% 时区偏移秒数
-spec timezone_offset(LocalTime :: calendar:datetime() | (Time :: non_neg_integer())) -> Seconds :: integer().
timezone_offset(LocalTimeOrTime) ->
	d_timezone:shift(LocalTimeOrTime).
timezone_offset() ->
	d_timezone:shift().

%% 格式化，格式 `<<"YYYY-MM-DDThh:mm:ss">>'
-spec format(LocalTime :: calendar:datetime() | (Time :: non_neg_integer())) -> binary().
format(LocalTimeOrTime) ->
	d_time:format(LocalTimeOrTime).

%% 格式化日期，格式 `<<"YYYY-MM-DD">>'
-spec format_date(calendar:date()) -> binary().
format_date(Date) ->
	d_time:format_date(Date).

%% 格式化时间，格式 `<<"hh:mm:ss">>'
-spec format_time(calendar:time()) -> binary().
format_time(HMS) ->
	d_time:format_time(HMS).

%% 格式化RFC3339，格式 `<<"YYYY-MM-DDThh:mm:ssTZD">>'，TZD = time zone designator (Z or +hh:mm or -hh:mm)
-spec format_rfc3339(LocalTime :: calendar:datetime() | (Time :: non_neg_integer())) -> binary().
format_rfc3339(LocalTimeOrTime) ->
	d_time:format_rfc3339(LocalTimeOrTime).

%% 格式化时区，格式 `<<"+hh:mm">> or <<"-hh:mm">>'
-spec format_timezone(integer()) -> binary().
format_timezone(OffsetSeconds) ->
	d_time:format_timezone(OffsetSeconds).

%% 设置系统时间
-spec set_localtime(calendar:datetime()) -> ok.
set_localtime(LocalTime) ->
	Seconds = localtime_to_time(LocalTime) - ?MODULE:time() + get_offset(),
	set_offset(Seconds).

%% 获取系统时间
-spec get_localtime() -> LocalTime :: calendar:datetime().
get_localtime() ->
	time_to_localtime(?MODULE:time()).

%% 时间加法：Time + Seconds
-spec time_add(non_neg_integer(), integer()) -> Time :: non_neg_integer().
time_add(Time, Seconds) when Seconds =:= 0 ->
	Time;
time_add(Time, Seconds) when Time < ?MIN_TIME ->
	Time + Seconds;
time_add(Time, Seconds) when Seconds < -?MIN_TIME ->
	%% 容错 Time1 - Time2 => time_add(Time1, -Time2)
	time_offset(-Seconds, Time);
time_add(Time, Seconds) when Seconds < ?MIN_TIME ->
	LocalTime = time_to_localtime(Time),
	NLocalTime = localtime_add(LocalTime, Seconds),
	localtime_to_time(NLocalTime).

%% 时间减法：Time1 - Time2
-spec time_sub(non_neg_integer(), non_neg_integer()) -> Seconds :: integer().
time_sub(Time1, Time2) when Time1 =:= Time2 ->
	0;
time_sub(Time1, Time2) when Time1 < ?MIN_TIME ->
	Time1 - Time2;
time_sub(Time1, Time2) when Time2 < ?MIN_TIME ->
	%% 容错 Time - Seconds => time_sub(Time, Seconds)
	%% 容错 Time + Seconds => time_sub(Time, -Seconds)
	time_add(Time1, -Time2);
time_sub(Time1, Time2) ->
	time_offset(Time2, Time1).

%% 相差秒数：TimeTo - TimeFrom
-spec time_offset(non_neg_integer(), non_neg_integer()) -> Seconds :: integer().
time_offset(TimeFrom, TimeTo) when TimeFrom =:= TimeTo ->
	0;
time_offset(TimeFrom, TimeTo) when TimeFrom < ?MIN_TIME; TimeTo < ?MIN_TIME ->
	TimeTo - TimeFrom;
time_offset(TimeFrom, TimeTo) ->
	LocalTimeFrom = time_to_localtime(TimeFrom),
	LocalTimeTo = time_to_localtime(TimeTo),
	localtime_offset(LocalTimeFrom, LocalTimeTo).

%% 当天0点：Time当天0点
-spec daytime(non_neg_integer()) -> Time :: non_neg_integer().
daytime(Time) ->
	daytime_add(Time, 0).
daytime() ->
	daytime(?MODULE:time()).

%% 当天偏移时间：Time当天0点 + Seconds
-spec daytime_add(non_neg_integer(), integer()) -> Time :: non_neg_integer().
daytime_add(Time, Seconds) when Time > ?MIN_TIME ->
	LocalTime = time_to_localtime(Time),
	NLocalTime = do_daytime_add(LocalTime, 0, Seconds),
	localtime_to_time(NLocalTime).

%% 当天偏移秒数：Time - Time当天0点
-spec daytime_offset(non_neg_integer()) -> Seconds :: integer().
daytime_offset(Time) when Time > ?MIN_TIME ->
	{_, HMS} = time_to_localtime(Time),
	do_daytime_offset(HMS, 0).

%% 当天0点相差天数：(TimeTo当天0点 - TimeFrom当天0点) / 86400
-spec daytime_offset_days(non_neg_integer(), non_neg_integer()) -> Days :: integer().
daytime_offset_days(TimeFrom, TimeTo) when TimeFrom =:= TimeTo ->
	0;
daytime_offset_days(TimeFrom, TimeTo) when TimeFrom > ?MIN_TIME ->
	LocalTimeFrom = time_to_localtime(TimeFrom),
	LocalTimeTo = time_to_localtime(TimeTo),
	do_daytime_offset_days(LocalTimeTo, 0) - do_daytime_offset_days(LocalTimeFrom, 0).
daytime_sub_days(TimeTo, TimeFrom) ->
	daytime_offset_days(TimeFrom, TimeTo).

%% 当天5点：Time当天5点
-spec daytime5(non_neg_integer()) -> Time :: non_neg_integer().
daytime5(Time) ->
	daytime5_add(Time, 0).
daytime5() ->
	daytime5(?MODULE:time()).

%% 当天偏移时间：Time当天5点 + Seconds
-spec daytime5_add(non_neg_integer(), integer()) -> Time :: non_neg_integer().
daytime5_add(Time, Seconds) when Time > ?MIN_TIME ->
	LocalTime = time_to_localtime(Time),
	NLocalTime = do_daytime_add(LocalTime, 5, Seconds),
	localtime_to_time(NLocalTime).

%% 当天偏移秒数：Time - Time当天5点
-spec daytime5_offset(non_neg_integer()) -> Seconds :: integer().
daytime5_offset(Time) when Time > ?MIN_TIME ->
	{_, HMS} = time_to_localtime(Time),
	do_daytime_offset(HMS, 5).

%% 当天5点相差天数：(TimeTo当天5点 - TimeFrom当天5点) / 86400
-spec daytime5_offset_days(non_neg_integer(), non_neg_integer()) -> Days :: integer().
daytime5_offset_days(TimeFrom, TimeTo) when TimeFrom =:= TimeTo ->
	0;
daytime5_offset_days(TimeFrom, TimeTo) when TimeFrom > ?MIN_TIME ->
	LocalTimeFrom = time_to_localtime(TimeFrom),
	LocalTimeTo = time_to_localtime(TimeTo),
	do_daytime_offset_days(LocalTimeTo, 5) - do_daytime_offset_days(LocalTimeFrom, 5).
daytime5_sub_days(TimeTo, TimeFrom) ->
	daytime5_offset_days(TimeFrom, TimeTo).

%% 当周周一0点：Time当周周一0点
-spec weektime(non_neg_integer()) -> Time :: non_neg_integer().
weektime(Time) ->
	weektime_add(Time, 0).
weektime() ->
	weektime(?MODULE:time()).

%% 当周偏移时间：Time当周周一0点 + Seconds
-spec weektime_add(non_neg_integer(), integer()) -> Time :: non_neg_integer().
weektime_add(Time, Seconds) when Time > ?MIN_TIME ->
	LocalTime = time_to_localtime(Time),
	WeekSeconds = seconds_of_week(LocalTime, 0),
	NLocalTime = do_daytime_add(LocalTime, 0, Seconds - WeekSeconds),
	localtime_to_time(NLocalTime).

%% 当周偏移秒数：Time - Time当周周一0点
-spec weektime_offset(non_neg_integer()) -> Seconds :: integer().
weektime_offset(Time) when Time > ?MIN_TIME ->
	{Date, HMS} = time_to_localtime(Time),
	seconds_of_week({Date, HMS}, 0) + do_daytime_offset(HMS, 0).

%% 当周周一5点：Time当周周一5点
-spec weektime5(non_neg_integer()) -> Time :: non_neg_integer().
weektime5(Time) ->
	weektime5_add(Time, 0).
weektime5() ->
	weektime5(?MODULE:time()).

%% 当周偏移时间：Time当周周一5点 + Seconds
-spec weektime5_add(non_neg_integer(), integer()) -> Time :: non_neg_integer().
weektime5_add(Time, Seconds) when Time > ?MIN_TIME ->
	LocalTime = time_to_localtime(Time),
	WeekSeconds = seconds_of_week(LocalTime, 5),
	NLocalTime = do_daytime_add(LocalTime, 5, Seconds - WeekSeconds),
	localtime_to_time(NLocalTime).

%% 当周偏移秒数：Time - Time当周周一5点
-spec weektime5_offset(non_neg_integer()) -> Seconds :: integer().
weektime5_offset(Time) when Time > ?MIN_TIME ->
	{Date, HMS} = time_to_localtime(Time),
	seconds_of_week({Date, HMS}, 5) + do_daytime_offset(HMS, 5).
%%%===================================================================
%%% Internal functions
%%%===================================================================

do_daytime_add({Date, {Hour, _, _}}, DayHour, Seconds) when Hour >= DayHour ->
	localtime_add({Date, {DayHour, 0, 0}}, Seconds);
do_daytime_add({Date, _}, DayHour, Seconds) ->
	localtime_add({Date, {DayHour, 0, 0}}, Seconds - ?SECONDS_PER_DAY).

do_daytime_offset({Hour, Minute, Second}, DayHour) when Hour >= DayHour ->
	calendar:time_to_seconds({Hour - DayHour, Minute, Second});
do_daytime_offset({Hour, Minute, Second}, DayHour) ->
	calendar:time_to_seconds({Hour - DayHour, Minute, Second}) + ?SECONDS_PER_DAY.

do_daytime_offset_days({Date, {Hour, _, _}}, DayHour) when Hour >= DayHour ->
	calendar:date_to_gregorian_days(Date);
do_daytime_offset_days({Date, _}, _DayHour) ->
	calendar:date_to_gregorian_days(Date) - 1.

seconds_of_week(LocalTime, DayHour) ->
	DayNum = (do_daytime_offset_days(LocalTime, DayHour) + 5) rem 7 + 1,
	(DayNum - 1) * ?SECONDS_PER_DAY.

cache_time(_FuncName, _Key, _Time) -> ok.
get_cache_time(_FuncName, _Key) -> ?FALSE.

next_sunday_end_time() ->
	weektime_add(?MODULE:time(), ?SECONDS_PER_WEEK - 1).

%% 开始时间
day_start_second() ->
	daytime().
day_start_second(Time) ->
	daytime(Time).

%% 是否为同一天
is_same_day(Seconds1, Seconds2) ->
	{Date1, _} = time_to_localtime(Seconds1),
	{Date2, _} = time_to_localtime(Seconds2),
	Date1 == Date2.
is_same_week(Seconds1, Seconds2) ->
	week_number(Seconds1) == week_number(Seconds2).

%% 时间戳转日期
%% ret {{YY,MM,DD},{H,M,S}}
timestamp_to_datetime(TimesTamp) ->
	time_to_localtime(TimesTamp).

%% 日期转时间戳
datetime_to_timestamp(DateTime) ->
	localtime_to_time(DateTime).

%% 比较两个时间相差天数 0则是同一天
day_diff({Date1, _}, {Date2, _}) ->
	day_diff(datetime_to_timestamp({Date1, {0, 0, 0}}), datetime_to_timestamp({Date2, {0, 0, 0}}));
day_diff(Time1, Time2) ->
	daytime_offset_days(Time2, Time1).

%% 返回0点的时间戳
midnight() ->
	daytime().
midnight(Timestamp) ->
	daytime(Timestamp).

now_seconds() ->
	daytime_offset(?MODULE:time()).