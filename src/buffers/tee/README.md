# Single-Queue Tee Buffer

This is an in memory queue using the Erlang IPC Mailbox system to serialize
incoming/outgoing event requests and a `gen_server` for stateful storage. If
multiple processors register as 'takers' they will all get a consistent view
of the queue (i.e. rather than First-Come-First-Serve, each processor will get
the same events in order). The 'givers' use a single broadcast server to
serialize all incoming events to all takers.

Note, this has understandably poor performance when there is a large number of
noisy Monitors as they start to generate contention for the broadcast server.

## Configuration:

This tee buffer has no configuration settings.

## Example Usage:

Create a tee buffer:

```erlang
> {ok, _, Buffer} = libemp_buffer:start( libemp_tee_buffer ).
```

Create a tee buffer which optimistically batches:

```erlang
> {ok, _, Buffer} = libemp_buffer:start( libemp_tee_buffer, [
        {buffer_take_count, all}
]).
```