%% Hang Sink -
%%
%%    Useful for testing parallism in the stack runtime. Hangs for a particular
%%    amount of time before allowing the event to pass. If you want to block
%%    events from going forward within a window of time, use the Shape Sink.
%%
-module(libemp_hang_sink).
-behaviour(libemp_sink).

-export([setup/1,process/3]).

%% @doc Check if the arguments override the time to hang for, error on bad args.
setup( Args ) -> build_state( Args ).

%% @doc Just hang for the specified time and let the event continue down the stack.
process( _Event, _Buffer, #{hang := Time}) ->
  timer:sleep(Time),
  next.

%% @hidden
%% @doc Actually build our hang state, which maintains the time to hang for.
build_state([]) ->
  {ok, #{hang => 1000}}; % Default to hang for 1 second.
build_state([Time]) when is_integer(Time) andalso Time > 0 ->
  {ok, #{hang => Time}};
build_state( Badarg ) ->
  {error, {badarg, Badarg}}.
