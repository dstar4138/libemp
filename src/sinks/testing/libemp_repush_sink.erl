%%% Repush Sink -
%%%
%%%   Tests the ability of the sinks to push events back onto the Buffer.
%%%
-module(libemp_repush_sink).
-behaviour(libemp_sink).

%% API
-export([process/3]).

%% @doc Push the event to the new buffer.
process( Event, Buffer, _State ) ->
  libemp_buffer:give( Event, Buffer ),
  next.

