# LibeEMP Event Monitors

LibEMP Event Monitors represent two things:

1. *An Event Producer.* They are the source of all events in the system, they
 can represent a downstream service, a neuron in distress, a feedback mechanism,
 etc.

2. *A Local Actor.* As wrappers around downstream services, they act as the
 local host's liaison. As wrappers around neurons, feedback mechanisms, or
 simple processes, they act as the Action sink to effect their local state.

This means event monitors are services in their own right, but who's primary
goal is to facilitate communication to-and-from outside of the LibEMP Node.
As a *Sympathetic Agent*, the Monitor is an Actor which interacts with the world
as the local hand for the resident Node.

## Monitor Implementation Layout

Monitor configuration differs from Buffers in that Monitors can be as full-blown
as any other Erlang application, or as minimal as a simple module used to wrap
some RESTful service. As such we do not dictate much about their implementation,
we follow a couple standards to keep things relatively consistent (at least for
the provided example Monitors:

* `libemp_*_monitor.erl` - This is the `libemp_monitor` behaviour
    implementation. Several implementations utilize multiple module to separate
    out functionality considerations (such as those implemented as service
    wrappers or NIFs).

* `README.md` - A read me with the monitor implementation description and
    usability requirements, along with a description of the possible
    configurations possible.

## Monitor Usage

Many Monitors may be regular Erlang applications, which may mean just starting
them in the normal way (i.e. `application:start/1,2`). Or similar to the
following, note that it requires a reference to a buffer object. The monitor
wrapper will register itself.

```erlang
> {ok, MonitorPid} = libemp_monitor:start( libemp_timer_monitor, [], BufferRef ).
```

However, if you want to let LibEMP keep track of the Monitor and wire it up to
a Buffer for you, you can start up the monitor with the LibEMP.

```erlang
> ok = libemp:start( [ {monitor, libemp_timer_monitor, []} ] ).
```

This will create a default buffer and wire the monitor up to it (since there
wasn't one specified).
