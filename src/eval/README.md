# LibEMP Usage Examples:

LibEMP is a library for general monitoring platforms, but at its core it is an
event stream router. This makes it perfect for a wide range of uses. We 
demonstrate how to use LibEMPs feature set with a number of example modules.

You can run these modules by launching the debug shell (i.e. `make shell`), and 
follow the directions for each subsection.

## Hand-Wiring Monitors to Sinks:

The concept of wiring can be convoluted without a concrete example. The
[eval/hand_wired](https://github.com/dstar4138/libemp/blob/develop/src/eval/hand_wired.erl)
module shows you a number of ways to start up a full monitoring platform (e.g. a
monitor, a buffer, and a sink wrapped in a processor).

To get a Sink to process the events of a Monitor, they both have to be tied to
the same Buffer. This facilitates the serialization, storage, and propagation
of events. Additionally, a Sink by itself does not implement the mechanisms to
read from a Buffer, which is why a Processor needs to be involved.

Lets look at how to wire this up by hand:

```erlang
{ok, _Ref, Buffer} = libemp_buffer:start( libemp_simple_buffer, [] ),
{ok, Proc} = libemp_processor:start_link( Buffer, libemp_logger_sink, [] ),
{ok, Monitor} = libemp_monitor:start_link( libemp_timer_monitor, [], Buffer ),
```

A Buffer is started first, once created we create a processor to pull events off
and give to a provided sink. We can then populate the buffer with events, by 
linking a Monitor. The LibEMP Application can automate this wiring and provide
fault-tolerance (i.e. restarting after failures, logging errors, etc.).

```erlang
  libemp:wire( myapp, [
    {buffer, mybuffer, {libemp_simple_buffer, []}},
    {monitor, mymonitor, {libemp_timer_monitor, []}, mybuffer},
    {sink, mysink, {libemp_logger_sink, []}},
    {stack, [mysink], mybuffer}
  ] ).
```

Note by doing it this way you must name the application and you loose easy 
access to the buffer and/or processor. You can experiment with Buffers and 
Processors with `hand_wired` by sending them events directly.

```erlang
> {ok, Buffer, Processor} = hand_wired:wired_processor().
...
> hand_wired:push_events_to_buffer( Buffer, 5 ).
...
=INFO REPORT==== 14-Feb-2016::13:20:50 ===
EVENT: {mybuffevent,5}

> hand_wired:push_events_to_processor( Processor, 5 ).
...
=INFO REPORT==== 14-Feb-2016::13:20:50 ===
EVENT: {myprocevent,5}

```

## Stacking Sinks:

Processors, while only able to wrap one sink, can wrap a Stack which allows for
concurrent processing of events. Note that if Multiple Processors are attached 
to a Buffer, there may be inconsistent state between multiple instantiations of
a Sink.

Wiring stacks by hand is complicated, but you can see how that works via the
[eval/sink_stacking](https://github.com/dstar4138/libemp/blob/develop/src/eval/sink_stacking.erl)
module. Namely, it involved configuring a `libemp_stack_sink` with the 
configurations of all the sinks you wish to load beneath it. Note that 
sub-stacks run in parallel with one another but any sink in a stack will run
in serial.

```erlang
> sink_stacking:run_timed().
```

## Event Generation from Sinks:

Sinks can also generate new events due to events they see. To demo this we have
a module which will push the event it gets back onto the buffer, in effect 
running forever. See 
[eval/sink_repush](https://github.com/dstar4138/libemp/blob/develop/src/eval/sink_repush.erl)
for details.

```erlang
> sink_repush:infinite_run().
```
