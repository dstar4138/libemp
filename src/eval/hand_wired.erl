-module(hand_wired).

%% API
-export([via_app/0,via_hand/0]).
-export([wired_processor/0]).
-export([stand_up_event_handlers_only/0]).
-export([
  push_events_to_processor/2,
  push_events_to_buffer/2
]).

%%%==================================================================
%%% Full End to End wiring Examples
%%%==================================================================

%% @doc Wire up a monitor, buffer, and sink via the App.
via_app() ->
  libemp:wire( myapp, [
    {buffer, mybuffer, {libemp_simple_buffer, []}},
    {monitor, mymonitor, {libemp_timer_monitor, []}, mybuffer},
    {sink, mysink, {libemp_logger_sink, []}},
    {stack, [mysink], mybuffer}
  ] ).

%% @doc By hand this is how we wire up monitors and processors to buffers.
via_hand() ->
  {ok, _Ref, Buffer} = libemp_buffer:start( mybuffer, libemp_simple_buffer, [] ),
  {ok, Proc} = libemp_processor:start_link( Buffer, libemp_logger_sink, [] ),
  {ok, Monitor} = libemp_monitor:start_link( mymonitor, libemp_timer_monitor, [], Buffer ),
  {ok, Buffer, Proc, Monitor}.

%%%==================================================================
%%% Event Propagation Mechanisms
%%%==================================================================

%% @doc Test just the event handling side of the spectrum. This will hand wire
%%    a new processor to a new buffer and push five events to the buffer to
%%    watch them percolate. It then pushes five events to the processor itself.
%% @end
stand_up_event_handlers_only() ->
  Return = wired_processor(),                  % Create the Buffer/Sink
  {ok, Buffer, Processor} = Return,
  push_events_to_buffer( Buffer, 5 ),          % Push events to the buffer.
  push_events_to_processor( Processor, 5 ),    % Then bypass it & push to sink.
  Return.

%% @doc Create a buffer, and wire it to a processor wrapping a logger sink.
wired_processor() ->
  {ok, _Ref, Buffer} = libemp_buffer:start( libemp_simple_buffer ),
  {ok, Proc} = libemp_processor:start_link( Buffer, libemp_logger_sink, [] ),
  {ok, Buffer, Proc}.

%% @doc Push any event to a buffer directly. This registers the local process
%%   as a 'giver' and pushes a number of fake events, then unregisters itself.
%% @end
push_events_to_buffer( Buffer, N ) ->
  {ok, Giver} = libemp_buffer:register( give, Buffer ),
  lists:foldl( fun(I,_) -> libemp_buffer:give( {mybuffevent,I}, Giver ) end,
               ok, lists:seq(1,N) ),
  libemp_buffer:unregister( Giver ).

%% @doc Bypass any buffer and push the events directly to a particular processor
%%    to handle when it is ready.
%% @end
push_events_to_processor( Proc, N ) ->
  Events = lists:map(fun (I) -> {myprocevent, I} end, lists:seq(1,N)),
  libemp_processor:push( Events, Proc ).

