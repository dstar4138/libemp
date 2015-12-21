-module(hand_wired).

%% API
-export([run/0,wired_processor/0, trigger_multiple_events/2]).

%% Start up a buffer and processor, and then send events through the buffer
%% to watch the logger sink trigger.
run() ->
  {ok,Buf,_} = wired_processor(),                   % Create the buffer/sink.
  {ok,Buffer} = libemp_buffer:register(give, Buf),  % Register with buffer too.
  trigger_multiple_events(Buffer, 10),              % Send 10 events through it.
  Buffer.                                           % Return buffer to user.

%% @hidden
%% @doc Create a buffer, and wire it to a processor wrapping a logger sink.
wired_processor() ->
  {ok, _Ref, Buffer} = libemp_buffer:start(libemp_simple_buffer),
  Res = libemp_processor:start_link( Buffer, libemp_logger_sink, []),
  {ok, Buffer, Res}.

%% @hidden
%% @doc Given Buffer object, send N events through it.
trigger_multiple_events( Buffer, N ) ->
  lists:foldl( fun( I, _ ) ->
    libemp_buffer:give( {mytestevent, I}, Buffer )
  end, ok, lists:seq(1,N) ).