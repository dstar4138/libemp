# Lib EMP #

EMP stands for Extensible Monitoring Platform, and libemp is the core dynamic
PubSub engine. It is generic enough to be repurposed for alternative event
driven applications, but has been specialized for the purpose of EMP. For this
reason it is not recommended for a system reliant on real-time or static event
evaluation.


## Functionality ##

There are two key features libemp provides:

### Dynamic Event Subscriptions ###

Instead of run-time analysis of each event stream, we instead utilize a
compile-time restructuring of the subscription unification function. This
compile-time cost happens on the introduction of any new subscription added via
the subscription intermediate language or `add_subscription/2` function.

### Plug-and-Play Event Streams ###

Plug-ins can both publish one or more event streams and subscribe to the
event streams of others. An event stream is a single type of event, who's
parameters contain actionable data. For example, a Github plugin can have an
event stream devoted to "merge" events, and another for "pull". These events
could contain parameters denoting the repository and user which triggered them.


## Usage ##

libemp can only be accessed inside of an Erlang VM currently, as such, we must
initialize Erlang supervision tree for the event processor and then all
plug-ins. Namely,

    > libemp:start().
    > Plugins = [ github, moduleA ].
    > libemp:load( Plugins ).

When a new subscription needs to be added:

    > NewSub = { mergeEvent, 
                 fun(E) -> event:param(user,E) =:= "dstar4138" end,
                 commandX,
                 fun(E) -> [ event:param(user,E),
                             event:param(message,E)
                           ]
                 end
               }.
    > moduleA:module_info( attributes ).
    [{vsn, ...1}]
    > libemp:add_subscription( moduleA, NewSub ).
    > moduleA:module_info( attributes ).
    [{vsn, ...2}]

This could recompile the `moduleA:unify/1` module to look like:

    unify( Event ) -> 
        case type(Event) of
        ...
        mergeEvent -> (case user(Event) of
                        ...
                        "dstar4138" -> 
                            erlang:apply(moduleA,commandX,[user(Event),
                                                           message(Event)])
                        ...
                       end)
        ...
        end.

Please see the [empd](https://github.com/Empd/empd) repository for more
examples and use cases. You may include libemp in your project using 
[rebar](https://github.com/rebar/rebar).
