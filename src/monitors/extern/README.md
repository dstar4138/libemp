# External Monitor

The External program monitor is a simple mechanism for monitoring an 
external program written in any language. By default it is set up so 
that each line of output is a new event. This can be overridden to 
be a binary protocol (see port configuration [1]).

[1] - http://erlang.org/doc/man/erlang.html#open_port-2

## Events:

The event structure produced by this monitor is an Erlang tuple:

* `{port_event, Name, Data}` - Where `Name` is the the monitor's name, 
 as specified by the wiring, or overridden by the `extern_name` 
 configuration.

## Configuration:

Extern Monitor can take three configs, only the `command` is required:

* `{command, string() | binary()}` - The command to run on the 
 command line. It will be passed in directly into the `open_port`
 function.
* `{extern_name, atom()}` - The name used in the port event. 
* `{port_settings, [ term() ]}` - The override settings for the port.

## Example Usage:

Via the wiring configuration, you can simply do the following:

```erlang
{monitor, testpy, {libemp_extern_monitor, [
    {command, "./priv/test.py"}
]}}.
```

Or you can override the port parameters, to use a binary protocol.
For example, if you wanted to use Erlectricity [2].

```erlang
{monitor, myprogram, {libemp_extern_monitor, [
    {command, "ruby ./priv/myprogram.rb"},
    {extern_name, myprogram},
    {port_settings, [
        {packet, 4}, nouse_stdio, exit_status, binary
    ]}
]}}.
```

If you want to launch the monitor by hand (not recommended), you can 
do the following:

```erlang
> {ok, _Pid, BufferRef} = libemp_buffer:start( ... ).
> {ok, Pid} = libemp_monitor:start_link( libemp_extern_monitor, [
        {command, "./priv/test.py"}
    ], BufferRef ).
```

[2] - https://github.com/mojombo/erlectricity
