# LibEMP Local Node Monitor

This Monitor is a simple monitor for catching events that the local node emits.
If another Monitor pushes a `log` or there are changes in the local node (new 
or removals of buffers/monitors/etc). 

## Events:

The event structure produced by this monitor is an Erlang tuple. There are a 
few strict types of events, but this is variable and may change in the future:

* `{log, Level, Location, Message}` 
* `{node, new, monitor | app | proc | buffer, Details}`
* `{node, delete, monitor | app | proc | buffer, Details}`
* `{node, generic, GenericEvent}`

## Configuration:

There are no configurations for this monitor. You should only start this with 
the SYS application.