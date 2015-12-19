-module(sink_stacking).

%% API
-export([run/0,run/1]).

run() -> run({mytestevent, "Hello World"}).
run(MockEvent) ->
  StackSink = create_stack([
    {libemp_stack_sink, [{stack,[
      {libemp_logger_sink, []},  % Log the new event,
      {libemp_context_sink, []}, % Add context to it,
      {libemp_logger_sink, []}   % the log it again to show it works.
    ]}]},
    {libemp_logger_sink,[]} % Show that any context within the substack is lost.
  ]),
  MockBuffer = [],
  libemp_sink:process(MockEvent, MockBuffer, StackSink).

create_stack(Sinks) ->
  {ok, Stack} = libemp_sink:setup(libemp_stack_sink, [{stack,Sinks}]),
  Stack.