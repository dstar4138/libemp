# LibEMP Applications

LibEMP Applications can be defined with `wire` or `wire_script` files. These
files define the Buffers, Monitors, and Sinks that will be created and wired
together.

LibEMP bundles a couple applications that can be started on launch:

* `sys` - The SYS Application, builds the system event stream. Will listen and
    handle Local Node events.
* `default` - The Default Application. Builds the default buffer/processor that
    apps can build off of.

## Example Usage:

To load up the provided applications, you can wire them up like any other
LibEMP App:

```erlang
> libemp:wire( sys, "./src/apps/sys.wire" ).
ok
=INFO REPORT==== 14-Feb-2016::01:38:51 ===
EVENT: {node,new,monitor,node}

=INFO REPORT==== 14-Feb-2016::01:38:51 ===
EVENT: {node,new,application,sys}
```

This will start up the local system node monitoring application. Note that two 
events will be logged to `error_logger`, that a new monitor called `node` has
been started, and a new application `sys` was started. You test this App out
further by starting any monitor and seeing what the system does to accommodate:

```erlang
> libemp:wire( example, [{monitor, libemp_timer_monitor}] ).
ok
=INFO REPORT==== 14-Feb-2016::01:43:15 ===
EVENT: {node,new,buffer,default}

=INFO REPORT==== 14-Feb-2016::01:43:15 ===
EVENT: {node,new,monitor,libemp_timer_monitor}

=INFO REPORT==== 14-Feb-2016::01:43:15 ===
EVENT: {node,new,application,example}
```