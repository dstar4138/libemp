# LibEMP Event Sinks

LibEMP Event Sinks encapsulate the processing of events. They are potentially
stack-able which means they are capable of being used as a work-flow triggered
by the events sent to a Buffer. They need to be wired up to initialize this
flow upfront into a Processor Stack which is in theory stateless enough to be
replicated and load balanced across local cores at the very least.

Each of the subdirectories contained herein implement example and useful Sinks
which can be used in your own Work-Flows. Since Sinks contain the majority of
your business logic we leave their implementation up to you and provide an
extremely simple API and user interface.

## Sink Implementation Layout

We are attempting to keep things consistent in our bundled Sink implementations.
For each directory, `*`, you will see at least two files:

* `libemp_*_sink.erl` - This is the `libemp_sink` behaviour implementation.
    Several implementations utilize multiple modules to separate functional
    considerations (such as those implemented as a service wrapper or NIFs).
* `README.md` - A read me with a description of the Sink including the usability
    and configuration requirements.

The exception is the `testing` directory; which has several sink implementations
all for testing Buffers and Monitors. Feel free to use these for development
purposes.

## Sink Usage

The Sinks themselves all have unique configuration mechanisms, as such we do not
dictate much here. Additionally, Sinks by themselves do not provide much
functionality beyond their individual business logic (as they should).
Therefore, we provide an implementation of a 'Processor' which wraps the Sink
in the wiring required to connect to a Buffer.

First however, when testing a Sink, we do not need the wiring in place:

```erlang
> {ok, Sink} = libemp_sink:setup( libemp_logger_sink, [] ).
> libemp_sink:process( {my,test,event}, fakebuffer, Sink ).
```

Note that we pass in the atom `'fakebuffer'` into the sink. As the 'logger' sink
does not utilize a buffer (e.g. to generate more events, etc), it is safe to
put anything there.

Otherwise, if you want to test it with a buffer, you can do the following:

```erlang
> {ok, _Ref, Buffer} = libemp_buffer:start( libemp_simple_buffer, [] ).
> libemp_processor:start_link( Buffer, libemp_logger_sink, [] ).
> {ok, BufRef} = libemp_buffer:register( give, Buffer ).
> libemp_buffer:give( {my,test,event}, BufRef ).
```

## Example Sinks:

We provide a couple Sinks which are easy to use and integrate in with your
applications:

#### Context

A simple context appender appends details like the timestamp of the event, and
the quantity of events it has seen prior (which are similar in 'type').

#### Logger

A simple Sink which pushes all events to the
[Erlang Error Logger](http://www.erlang.org/doc/man/error_logger.html). There
are no configurations for this currently, but future changes will migrate this
over to a configurable logging backend.

#### Shape

The Shape Sink acts as a traffic shaping Sink to reduce the number of
events needed to process later in the stack. It uses a typer function
given to it in order to classify the event. Once classified it will
keep track of the number of times it has seen that type within a window.
If the number of times reaches a threshold, it will let the newest
event through.

#### Stack

This Sink allows for parallel computation of events that the processor picks off
the Buffer. A Stack allows for the configuration of Sinks which will run in
sequence by default, however if the sub-sink is a Stack, it will throw that off
into a separate thread to compute (as Stacks' context can't leave the Stack).

```erlang
...
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
...
```