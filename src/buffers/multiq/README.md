# Multi-Queue Buffer

This LibEMP Event Buffer implements a unique queue per Monitor component. Thus,
when a Sink wishes to `take` from the Buffer, it round-robin selects a queue
from a monitor and trades it with an empty one. This has the effect of a 
non-consistent time `take`, and a non-sequentialized event processing, but earns
minimal read contention and batched event processing, reaching the theoretical
limit of Erlang's built-in `gen_server` message handling.

## Configuration:

There are no possible changes in configuration for this buffer.

## Example Usage:

There is nothing special about swapping this buffer out:

```erlang
> {ok, _, Buffer} = libemp_buffer:start( libemp_multiq_buffer ).
> {ok, BufferTakeRef} = libemp_buffer:register( take, Buffer ).
> {ok, BufferGiveRef} = libemp_buffer:register( give, Buffer ).
> ok = libemp_buffer:give( Event, BufferGiveRef ).
> {ok, [Event]} = libemp_buffer:take( BufferTakeRef ).
```

