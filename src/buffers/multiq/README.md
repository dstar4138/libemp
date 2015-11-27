# Multi-Queue Buffer

This LibEMP Event Buffer implements a unique queue per Monitor component. Thus,
when a Sink wishes to `take` from the Buffer, it round-robin selects a queue
from a monitor and trades it with an empty one. This has the effect of a 
non-consistent time `take`, and a non-sequentialized event processing, but earns
minimal read contention and batched event processing, reaching the theoretical
limit of Erlang's built-in `gen_server` message handling.

## Configuration

There are no possible changes in configuration for this buffer.

## 


