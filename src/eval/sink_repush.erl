-module(sink_repush).
-export([infinite_run/0]).

-define(EVENT, {mytestevent, "Hello World"}).
-define(STACK(Sinks),[{stack,Sinks}]).

%% Creates a stack like:
%%    [ context, log, hang, repush ]
%% Which has the effect of running forever. It will create a buffer, and push
%%  a single event to it. The stack will read from it, add context, log it, and
%%  then add the event back onto the buffer.
infinite_run() ->
  {ok, _Ref, Buffer} = libemp_buffer:start(libemp_simple_buffer),
  _Res = libemp_processor:start_link( Buffer, libemp_stack_sink, ?STACK([
      {libemp_context_sink, []},
      {libemp_logger_sink, []},
      {libemp_hang_sink, [1000]},
      {libemp_repush_sink, []}
    ])
  ),
  {ok, Giver} = libemp_buffer:register( give, Buffer ),
  libemp_buffer:give(?EVENT, Giver).
