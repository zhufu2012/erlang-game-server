%%%-------------------------------------------------------------------
%%% @author xiexiaobo@d-game.cn
%%% @copyright (C) 2020, DoubleGame
%%% @doc
%%% 日志头文件
%%% @end
%%% Created : 2020-01-22 10:00
%%%-------------------------------------------------------------------
-author('xiexiaobo@d-game.cn').

-ifndef(logger_hrl).
-define(logger_hrl, true).

-define(LOG_ALERT(String), ?DO_LOG(alert, [String])).
-define(LOG_ALERT(Format, Args), ?DO_LOG(alert, [Format, Args])).

-define(LOG_ERROR(String), ?DO_LOG(error, [String])).
-define(LOG_ERROR(Format, Args), ?DO_LOG(error, [Format, Args])).

-define(LOG_WARNING(String), ?DO_LOG(warning, [String])).
-define(LOG_WARNING(Format, Args), ?DO_LOG(warning, [Format, Args])).

-define(LOG_NOTICE(String), ?DO_LOG(notice, [String])).
-define(LOG_NOTICE(Format, Args), ?DO_LOG(notice, [Format, Args])).

-define(LOG_INFO(String), ?LOG_NOTICE(String)).
-define(LOG_INFO(Format, Args), ?LOG_NOTICE(Format, Args)).

-define(LOG_EVENT(String), ?DO_LOG(warning, [String, #{event => undefined}])).
-define(LOG_EVENT(Format, Args), ?DO_LOG(warning, [Format, Args, #{event => undefined}])).

-define(LOG_PROCESS_HEADER(Term), logger:set_process_metadata(#{header => Term})).

-define(LOG_STACKTRACE(Class, Reason, Stacktrace), d_logger:stacktrace(Class, Reason, Stacktrace)).

-define(LOCATION, #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE}).
-define(DO_LOG(Level, Args), case logger:allow(Level, ?MODULE) of
								 true -> apply(logger, macro_log, [?LOCATION, Level | Args]);
								 false -> ok
							 end).

%%-define(protocol_metrics(Msg, Expr), pmetrics:protocol(Msg, fun() -> Expr end)).
%%-define(metrics(Expr), pmetrics:run({?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, {}, fun() -> Expr end)).
-define(metrics(Expr), Expr).
%%-define(metrics_log(Expr, LogInfo), pmetrics:run({?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, LogInfo, fun() -> Expr end)).
-define(metrics_log(Expr, LogInfo), Expr).

-ifdef (mzx_debug).
-define(INFO_REPORT, "(FILE:~p  LIEN:~p)  ~n").
-define(DDEBUG(Args), ?LOG_WARNING("~p == ~p~n", [??Args, Args])).
-define(DDEBUG(Format, Args), ?LOG_ALERT(Format, [Args])).
-else.
-define(DDEBUG(Msg), void).
-define(DDEBUG(Desc, Msg), void).
-endif.


-endif. %% -ifndef
