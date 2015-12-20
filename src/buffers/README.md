# LibEMP Buffers

LibEMP Buffers encapsulate the transmission of events from event generators 
(LibEMP Monitors) and event processors (LibEMP Sinks). This transmission is 
transparent and wrapped in the API of a generic data structure (i.e. give/take
rather than push/pop or send/recv).

Each of the subdirectories contained herein implement a potential buffering 
mechanism. It is up to the user of the library to select and configure the 
buffer for utilization in their application. We can only make suggestions based
on potential workloads.

Some things to consider when picking a buffer implementation (note we do not 
provide a buffer for every edge case, you may need to roll your own).

* How many Monitors/Sinks are you running? (i.e. # of concurrent producers &
  consumers.)

* What is your expected burst and steady throughput from your particular 
  Monitors? (i.e. extremely vocal Monitors with 300K events per second?) You 
  may want to examine the Monitors' behaviour before attempting to select an 
  optimal Buffer.

* What are the Size of events from Monitors? (i.e. giant binary messages, or
  small tuples.)

* Is the parent application wanting to target distributed systems? (i.e. if so
  the distribution mechanism could be the Buffer.)

* Do you have multiple use-cases where multiple buffers may help segregate
  concerns? (i.e. two different work flows, one using a local buffer and the
  other being remote.)

We are not event queue experts nor high-throughput algorithm designers. 
That being said, we feel confident that our API design will support a wide
variety of implementations and mostly stay out of the way. However, if there are
areas for improvement or bugs, please let us know and feel free to contribute.
No contribution is too small!

## Buffer Implementation Layout

We are attempting to keep things consistent in our bundled Buffer 
implementations. For each directory, `*`, you will see at least two files:

* `libemp_*_buffer.erl` - This is the `libemp_buffer` behaviour implementation.
    Several implementations utilize multiple modules to separate functional 
    considerations (such as those implemented as service wrappers or NIFs). 
* `README.md` - A read me with a buffer implementation description and 
    usability requirements, along with a description of the possible
    configurations possible.
 
The exception is the `testing` directory; which has several buffer 
implementations all for testing Monitors and Sinks. Feel free to use these for
development purposes.

## System Buffer Configuration

The following configuration values are not expected by the Buffer initialization
system, but instead are kept consistent due for standardization purposes across
buffer implementations.

* `buffer_take_count` - the number of events a Sink may take at one time
    this number is an optimistic one and may not reflect the true number of 
    events pulled from the queue on average. For example, a setting of `50` 
    may only result in an average `take` of `2` if the Sink is faster than the
    Monitors. This configuration may also be `'all'`, which is extreme
    optimistic max-batch processing mode.

* `buffer_size` - if the buffer selection supports overflow or dropping, the 
    size constrains when that will start to happen.

Please look at the README for the particular buffer you are planning to use for
more configuration options.

## Buffer Usage

Using Buffers are fairly simple and contained to several module calls. Buffers 
are directional and must be initialized using the factory pattern, this factory
can be saved globally and used concurrently to generate a set of callbacks for
buffer access. We encapsulate this idea as a "registration" mechanism, where an
Actor may express intention.

First, the buffer can be started via `start/1,2,3` or `start_link/3` of the
`libemp_buffer` module. `start_link/3` can only be called if the LibEMP
framework has been started so that the buffer reference can be saved locally for
registration. For the following examples we will just use `start/2`, please see
the `libemp_buffer` module for more information on the other startup arguments.

```erlang
> {ok, _, BufferFactory} = libemp_buffer:start( libemp_simple_buffer, [] ).
```

Then, we can use the factory to register ourselves for a direction:

```erlang
> % Then per Monitor/Sink, get a new obj potentially...
> Direction = give. % | take
> {ok, ErlBufferObj} = libemp_buffer:register(Direction, BufferFactory).
> % ...
```

Next, we can use the Buffer object as a buffer, running the public Buffer API
on it:

```erlang
> {ok, Events} = libemp_buffer:take( ErlBufferObj ). % As long as a Taker
> ok = libemp_buffer:give( Event, ErlBufferObj ). % As long as a Giver
> {ok, Size} = libemp_buffer:size( ErlBufferObj ).
> ok = libemp_buffer:drop( ErlBufferObj, 10 ).
> ok = libemp_buffer:flush( ErlBufferObj ).
```

Finally, when we are done with the buffer and we wish to shut it down, we can
use the Buffer Factory or the Buffer Object itself to shut everything down
by calling `destroy/1`:

```erlang
> ok = libemp_buffer:destroy( BufferFactory ). % Or with the ErlBufferObj
```

However, if all we wanted to do was remove the taker/giver from the the buffer,
we can unregister it from the buffer:

```erlang
> ok = libemp_buffer:unregister( ErlBufferObj ).
```

Overall, we encourage LibEMP Buffer use due to compatibility with Erlang OTP
applications and the ease of plug-ability with the Event Monitor and Sink
implementations provided for free.

## Test Buffers:

Along with the provided functional buffers, we provide a number of testing
Buffers. These can be used to test a number of framework functionality:

#### Drop Buffer:

Useful for testing the LibEMP Buffer overhead. The Drop Buffer just drops all
events pushed to it. Attempting to get anything out of it will just return a
constant empty list. As such this provides the most minimal functionality a
Buffer can provide making it useful to test round-trip time into the LibEMP
framework and then back out to user-code.

```erlang
> {ok, _, Buffer} = libemp_buffer:start( libemp_drop_buffer ).
```

#### Logger Buffer:

Useful for testing LibEMP Monitors. All events going into the buffer are
broadcast into the Erlang `error_logger` and then forgotten. Attempting to get
anything out of it will just return a constant empty list. This provides a good
test for monitors.

```erlang
> {ok, _, Buffer} = libemp_buffer:start( libemp_logger_buffer, [
        {logger_fun, fun error_logger:info_msg/2}
  ]).
```

#### Pop Buffer:

Useful for testing LibEMP Sink stacks. All events going into the buffer are
forgotten, but attempting to get anything from the Buffer will generate a random
list of events given a set of constant events.

```erlang
> KnownEvents = [ {eventa, blah}, eventb, {eventc, 1, 2.0} ].
> {ok, _, Buffer} = libemp_buffer:start( libemp_pop_buffer, [
        {buffer_known_events, KnownEvents},
  ]).
```