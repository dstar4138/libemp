%%% Logging calls, we can introduce some indirection here. %%%
-define(LOG(Msg),error_logger:info_report(Msg)).
-define(LOG(Format,Data),error_logger:info_report(io_lib:format(Format,Data))).
-define(WARN(Msg),error_logger:warning_report(Msg)).
-define(WARN(Format,Data),error_logger:warning_report(io_lib:format(Format,Data))).
-define(ERROR(Msg),error_logger:error_report(Msg)).
-define(ERROR(Format,Data),error_logger:error_report(io_lib:format(Format,Data))).
