# LibEMP Applications

LibEMP Applications can be defined with `wire` or `wire_script` files. These
files define the Buffers, Monitors, and Sinks that will be created and wired
together.

LibEMP bundles a couple applications that can be started on launch:

* `sys` - The SYS Application, builds the system event stream. Will listen and
    handle Local Node events.
* `default` - The Default Application. Builds the default buffer/processor that
    apps can build off of.

