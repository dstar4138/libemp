% Public Types
-include("libemp.hrl").

% Debug Types
%%% Asserting and validation of running code, only use temporarily. %%%
-ifdef(DEBUG).
-define(ASSERT(TEST),case TEST of true -> pass; _ -> error(badassert) end).
-define(NOT_IMPLEMENTED( Ret ),
        error_logger:warning_report(
          io_lib:format("Not Implemented: ~p",[erlang:get_stacktrace()])),
        Ret
       ).
-else.
-define(ASSERT( _ ), pass).
-define(NOT_IMPLEMENTED( _ ),
        error(io_lib:format("Not Implemented: ~p",[erlang:get_stacktrace()]))).
-endif.

% Internal Logging Preferences:
%%% Logging calls, we can introduce some indirection here. %%%
-define(LOG(Msg),error_logger:info_report(Msg)).
-define(LOG(Format,Data),error_logger:info_report(io_lib:format(Format,Data))).
-define(WARN(Msg),error_logger:warning_report(Msg)).
-define(WARN(Format,Data),error_logger:warning_report(io_lib:format(Format,Data))).
-define(ERROR(Msg),error_logger:error_report(Msg)).
-define(ERROR(Format,Data),error_logger:error_report(io_lib:format(Format,Data))).
