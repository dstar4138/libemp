# Lib EMP #

[![Build Status](https://travis-ci.org/dstar4138/libemp.svg?branch=master)](https://travis-ci.org/dstar4138/libemp)

Building distributed systems is hard, it's worse without knowing all the trade 
offs you are implicitly making ahead of time. LibEMP targets a set of
distributed systems for handling event processing, and attempts to both ease
their creation as well as making these design decisions apparent and easier to
reason about.

EMP stands for Extensible Monitoring Platform, and LibEMP is the core dynamic
plug-in framework for event generation, routing, and processing. It handles the
fault-tolerance, scaling, and abstractions involved for building your own event
generators, routers, and processors.

## Building & Testing ##

Just run `make shell`. 
There are a number of Modules in `src/eval/*` which can be used
to test various functionality of LibEMP as a library. 
There are also a number of pre-built applications using LibEMP in the
[libemp_examples](https://github.com/dstar4138/libemp_examples/)
package which you can import.

## Background ##

In event processing, you typically have two to three components you concern 
yourself with: the event _sources_, their _processors_, and infrequently the 
message queues or _buffers_ that connect the two. Trade offs between these three
connected pieces are hard to make and are not frequently apparent in some 
systems. Stream processing applications, for example, typically ignore the 
topic of buffers and concern themselves with the _sources_ and _processors_, 
to provide yet another abstraction on top of these concepts. Not necessarily a 
bad idea, but can complicate things when it comes times to scale out or become
fault-tolerant (as these connections have been abstracted away).

LibEMP talks about all three components, with a few quirks and metadata
requirements for planning ahead in a distributed and fault-prone world. LibEMP
extends Erlang Nodes to include information regarding your application's 
structure. This is so the logic for handling Node/Network/Component faults can 
all be explicitly communicated.

### Sinks ###

Starting at the end of the equation, LibEMP Event _Sinks_ are used for 
encapsulating the processing of events. The functionality here is entirely up to
the developer of the application, that being said, Sinks can be composed and
stacked onto one another to easily extend their usefulness.

To learn by example, LibEMP bundles a number of Sinks which are general enough
to fit into your application, please see the
[Bundled Sinks](https://github.com/dstar4138/libemp/tree/develop/src/sinks).

### Buffers ###

The middle of the equation is the most interesting. LibEMP Event _Buffers_ are 
used for storing events waiting to be processed by the Event Sinks. While this
seems simple enough, the levels of trade off involved in that statement are
staggering. 

* Are adding events to the system transactional? 
* Are the events stored to disk?
* Do they need to be sent over a network?
* Do you need to sequence (order) the events being generated? 
* What are the speed/latency guarantees needed by the system?

Additionally, Buffers are used as the primary mechanism of distribution in a 
system. By choosing a distributed buffer, there could be a network of Sinks all 
processing from different Nodes, or simply separation of concerns for 
computationally intensive event processing.

To learn more about the Buffers currently in development and the trade offs 
they make, please see the 
[Bundled Buffers](https://github.com/dstar4138/libemp/tree/develop/src/buffers).

### Monitors ###

Finally, Event sources, which LibEMP calls _Monitors_, differ somewhat from the
common ideas around event stream sources. Monitors encapsulate entire APIs for 
downstream services. Instead of thinking in terms of "I want to monitor my 
GitHub events" think of it as a "GitHub Monitor which can be set to monitor 
your stream". This subtle difference allows for extreme scaling and fault 
tolerance later on.

It additionally adds a second level of interaction with your event processing
platform. If a Monitor wraps an entire service API, then it can also interface
with the external service from LibEMP's side. This allows LibEMP and its Event 
Sinks to affect the streams in turn (thereby closing the event loop). For 
example, when a Github 'fork' event happens on a set of particular repositories,
have the Sink automatically put a 'watch' on it.

To learn more about the Monitors currently in development, please see the
[Bundled Monitors](https://github.com/dstar4138/libemp/tree/develop/src/monitors).
