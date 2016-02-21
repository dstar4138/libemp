
-include("../../internal.hrl").

%% Default fault handling is to abort execution and let the processor log it.
-define(DEFAULT_FAULT_HANDLER, fun(_,_,Reason) -> {error, Reason} end).

%% State of a stack object.
-record(sink_state, { stack, handler }).

