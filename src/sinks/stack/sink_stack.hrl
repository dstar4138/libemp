
-include("../../internal.hrl").

%% Default fault handling is to abort execution and let the processor log it.
-define(DEFAULT_FAULT_HANDLER, fun(_,_,Reason) -> {error, Reason} end).
-define(DEFAULT_STACK, []).

%% State of a stack object.
-record(stack_state, {
          stack   = ?DEFAULT_STACK         :: libemp_sink_stack(),
          handler = ?DEFAULT_FAULT_HANDLER :: fault_handler()
}).

