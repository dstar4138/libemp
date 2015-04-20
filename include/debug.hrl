%%% Asserting and validation of running code, only use temporarily. %%%
-ifdef(DEBUG).
-define(ASSERT(TEST),case TEST of true -> pass; _ -> error(badassert) end).
-define(NOT_IMPLEMENTED( Ret ),
        error_logger:warning_report(io_lib:format("Not Implemented: ~p",[erlang:get_stacktrace()])),
        Ret
       ).
-else.
-define(ASSERT( _ ), pass).
-define(NOT_IMPLEMENTED( _ ),
            error(io_lib:format("Not Implemented: ~p",[erlang:get_stacktrace()]))).
-endif.
