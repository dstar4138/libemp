# Single-Queue Simple Buffer

This is the default LibEMP local buffer.

This is an in memory queue using the Erlang IPC Mailbox system to serialize
incoming/outgoing event requests and a `gen_server` for stateful storage. This
is used for testing and comparison to other buffer mechanisms due to its
simplicity.

## Configuration:

This simple buffer only has one possible override:

* `{buffer_take_count, all | pos_integer()}` - This buffer by default only
        pops one event at a time for `take/1` requests. This can be overridden
        with a larger number to indicate the max batch size, or `'all'` to
        indicate that we should be as optimistic as possible and grab as many
        events off the buffer as possible.

* `{buffer_take_hang, false | true}` - Tell the buffer to either hang until it
        can successfully take at least one, or else it will return immediately
        with an empty list. By default it will hang.

## Example Usage:

Create a simple buffer:

```erlang
> {ok, _, Buffer} = libemp_buffer:start( libemp_simple_buffer ).
```

Create a simple buffer which optimistically batches:

```erlang
> {ok, _, Buffer} = libemp_buffer:start( libemp_simple_buffer, [
        {buffer_take_count, all}
]).
```