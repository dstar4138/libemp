# LibEMP Buffers

Each of the subdirectories contained herein implement a potential buffering 
mechanism. It is up to the user of the library to select and configure the 
buffer for utilization in their application. We can only make suggestions based
on potential workloads.

Some things to consider when picking a buffer implementation (note we do not 
provide a buffer for every edge case, you may need to make your own).

* How many Monitors/Sinks are you running? (i.e. # of concurrent producers & 
  consumers.)

* What is your expected burst and steady throughput from your particular 
  Monitors? (i.e. extremely vocal Monitors with 300K events per second?) You 
  may want to examine the Monitors' behaviour before attempting to select an 
  optimal buffer.

* What are the Size of events from Monitors? (i.e. giant binary messages, or
  small tuples.)

* Is the parent application wanting to target distributed systems? (i.e. if so
  the distribution mechanism could be the Buffer, otherwise the Sink may help.)

We are not event queue experts nor high-throughput algorithm designers. 
Additionally, we feel confident that our API designs will support a wide 
variety of implementations and mostly stay out of the way. That being said,
if there are areas for improvement or bugs, please let us know and feel free 
to contribute. No contribution is too small!

## Buffer Implementation Layout

We are attempting to keep things consistent in our buffer implementations. In 
each directory, `*`, you will see at least two files:

* `libemp_*_buffer.cfg` - The buffer's default configuration. This is loaded 
    in on EUnit testing and on start up. Applications can override this 
    configuration through system configuration. There are fairly consistent
    naming schemes for system configs. See section below for system configs.
* `libemp_*_buffer.erl` - This is the `libemp_buffer` behaviour implementation.
    Several implementations utilize multiple modules to separate functional 
    considerations (such as those implemented as service wrappers or NIFs). 
* `README.md` - A read me with a buffer implementation description and 
    usability requirements.   
 
The exception is the `testing` directory; which has several buffer 
implementations all for testing Monitors and Sinks. Feel free to use these for
development purposes.

## System Buffer Configuration

The following configuration values are expected by the Buffer initialization 
system.

* `buffer_module` - an `atom` module name of the buffer implementation to load.

The following configuration values are not expected by the Buffer initialization
system, but instead are kept consistent due for standardization purposes:

* `buffer_take_count` - the number of events a Sink may take at one time
    this number is an optimistic one and may not reflect the true number of 
    events pulled from the queue on average. For example, a setting of `50` 
    may only result in an average `take` of `2` if the Sink is faster than the
    Monitors. This configuration may also be `all', which is extreme optimistic
    batch processing mode.

* `buffer_size` - if the buffer selection supports overflow or dropping, the 
    size constrains when that will start to happen.

