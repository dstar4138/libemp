# EPOCXY Buffer

Epocxy is a library written and maintained by Jay Nelson of DuoMark. Epocxy
encapsulates some best-practices they've found for reducing bottlenecks
when using Erlang. There is a talk about this with more detail than provided
here via Erlang-Factory: Avoiding Single Process Bottlenecks By Using Ets
Concurrency (PDF) [1]. It has a load more useful features for concurrency
management.

We use Epocxy's implementations of "lock-free" Ring/LIFO/FIFO queues to
implement simple LibEMP Buffers and provide a Plug-and-play wrapper for them.
This also provides a useful demonstration of how to wrap a third-party wrapper
implementation as a LibEMP Buffer.

[1] - http://www.erlang-factory.com/static/upload/media/1394716488140115jaynelson.pdf


## Configuration:

Epocxy buffers take up to three possible configurations when starting it up:

* `{buffer_type, ring | lifo | fifo}` - Select the buffer's type. By default it
    uses the FIFO implementation as that is the commonly expected functionality
    of a buffer.

* `{buffer_size, pos_integer()}` - Only relevant for `'ring'` buffers. This sets
    the size of the ring (i.e. if more events are added to the buffer than it's
    size, then it will begin to overlap).

* `{buffer_take_count, all | pos_integer()}` - The quantity of items to
    optimistically pop from the buffer. Thus if `'all'` is specified, the sink
    will get back a list of all available events to batch process on. On events
    where processing is very minimal, this tends to provide much better
    performance.

## Example Usage:

A FIFO queue which does optimistic max-batch processing (default):

```erlang
> {ok, _, FifoBuffer} = libemp_buffer:start( libemp_epoxcy_buffer, [] ).
```


A Simple Ring buffer of size `100`:

```erlang
> {ok, _, RingBuffer} = libemp_buffer:start( libemp_epoxcy_buffer, [
    {buffer_type, ring},
    {buffer_size, 100}
]).
```

A Simple LIFO buffer, which allows batch processing of `10` (i.e. `take/0`
operations optimistically return `10` items if possible, however it wont hang):

```erlang
> {ok, _, LifoBuffer} = libemp_buffer:start( libemp_epoxcy_buffer, [
    {buffer_type, lifo},
    {buffer_take_count, 10}
]).
```