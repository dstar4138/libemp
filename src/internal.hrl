% Private Types
-type libemp_event() :: term().
-type libemp_processor() :: pid() | atom().

-type monitor_config() :: term().
-record(monitorref, {
  name   :: string(),
  module :: atom(),
  config :: [ monitor_config() ],
  args   :: [ term() ],
  buffer :: libemp_buffer:libemp_buffer(),
  pid    :: atom() | pid(), % Can be a name or pid reference.
  linked :: pid()
}).

% Debug Types
%%% Asserting and validation of running code, only use temporarily. %%%
-ifdef(DEBUG).
-define(ASSERT(TEST),case TEST of true -> pass; _ -> error(badassert) end).
-define(NOT_IMPLEMENTED( Ret ),
        error_logger:warning_report(
          io_lib:format("Not Implemented: ~p",[erlang:get_stacktrace()])),
        Ret
       ).
-define(LOG(Msg),error_logger:info_report(Msg)).
-define(LOG(Format,Data),error_logger:info_report(io_lib:format(Format,Data))).
-else.
-define(ASSERT( _ ), pass).
-define(NOT_IMPLEMENTED( _ ),
        error(io_lib:format("Not Implemented: ~p",[erlang:get_stacktrace()]))).
-define(LOG(Msg),pass).
-define(LOG(Format,Data),pass).
-endif.

% Internal Logging Preferences:
%%% Logging calls, we can introduce some indirection here. %%%
-define(WARN(Msg),error_logger:warning_report(Msg)).
-define(WARN(Format,Data),error_logger:warning_report(io_lib:format(Format,Data))).
-define(ERROR(Msg),error_logger:error_report(Msg)).
-define(ERROR(Format,Data),error_logger:error_report(io_lib:format(Format,Data))).
