%%% LibEMP Monitor API -
%%%
-module(libemp_monitor_api).
-compile(inline).
-include("internal.hrl").

%% API
-vsn({0,0,1}).
-export([emit/2]).
-export([unlink/1]).
-export([log/3]).

%% @doc Push the event into the Buffer the monitor is linked to.
emit( Event, #monitorref{buffer=Buffer} ) ->
  libemp_buffer:give( Event, Buffer ).

%TODO: this is destructive, there should be a way to install a random process a Monitor.
%% @doc Unlink the Monitor from the LibEMP system.
unlink( #monitorref{buffer=Buffer, linked=Linked} ) ->
  libemp_buffer:unregister(Buffer),
  erlang:unlink(Linked). %TODO: remove from node table!

%% @doc Emit a log message into the LibEMP node. If the sys app is not up,
%%   these reroute to error_logger.
%% @end
log( Level, Message, #monitorref{name = Name} ) ->
  libemp_node_monitor:emit_log( Level, Name, Message ).

