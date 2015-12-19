-module(sink_stacking).

%% API
-export([run/0,run/1,run_timed/0]).

-define(EVENT, {mytestevent, "Hello World"}).
-define(STACK(Sinks),[{stack,Sinks}]).
-define(SUB_STACK(Sinks), {libemp_stack_sink, ?STACK(Sinks)}).

%% Create a stack like:
%%    [ [hang], [ log, context, log ], log ]
stack() ->
  {ok, Stack} = libemp_sink:setup(libemp_stack_sink, ?STACK([
    ?SUB_STACK([
      {libemp_hang_sink, [5000]} % Hang for 5 seconds.
    ]),
    ?SUB_STACK([
      {libemp_logger_sink, []},  % Log the new event,
      {libemp_context_sink, []}, % Add context to it,
      {libemp_logger_sink, []}   % the log it again to show it has been updated.
    ]),
    {libemp_logger_sink,[]} % Show that any context within the substack is lost.
  ])),
  Stack.

%%  which can run in sequence or in parallel:
%%    the substacks can be thrown into other threads to finish parsing while
%%    the parent stack continues with the rest of its processing (i.e. the last
%%    sink). Only when it hits a sink does it HAVE to run in sequence.
run() -> run(?EVENT).
run(MockEvent) -> libemp_sink:process(MockEvent, fakebuffer, stack()).

run_timed() ->
  io:format("Now building a stack of two substacks, 5 sinks in total.~n"
            "i.e. [ [hang], [ log, context, log ], log ]~n"),
   {Time, Stack} = timer:tc(fun stack/0),
  io:format("It took ~p μs.~n", [Time]),

  io:format("Then we'll go ahead and process an event and have one of the~n"
            "sinks hang for 5 seconds to simulate a long running process.~n"),
  {Time2, _Res} = timer:tc(libemp_sink, process, [?EVENT, fakebuffer, Stack]),
  io:format("It took ~p μs.~n", [Time2]),
  io:format("Thus overhead is: ~p μs.~n", [Time2-5000000]).

