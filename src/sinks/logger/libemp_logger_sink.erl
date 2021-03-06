%%% LibEMP Logger Sink - 
%%%  
%%%  A useful example of a very simple event sink. All events processed will be 
%%%  sent to the Erlang error_logger and passed unchanged to the next Sink in 
%%%  the stack.
%%%
-module(libemp_logger_sink).
-behaviour(libemp_sink).

-export([process/3]).

%% @doc Process an event by logging it's existence.
process( Event, _BufRef, _State ) ->
    %TODO: Switch over to lager.
    error_logger:info_report(io_lib:format("EVENT: ~p~n",[Event])),
    next.

