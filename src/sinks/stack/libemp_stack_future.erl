%%% Optimistic Concurrency for Stack Computation -
%%%
%%%   Stacks can parallelize their computation when running sub-stacks as the
%%%   modification of the event cannot leave the stack it was modified in.
%%%   Therefore, we create future objects to try and get that computation done
%%%   in the mean time while the rest of the stack computes or waits.
%%%
-module(libemp_stack_future).

%% API
-export([new/2,join/1]).

-ifdef(DEBUG).
new( Fun, Args ) when is_list(Args) andalso is_function(Fun, length(Args)) ->
  catch erlang:apply(Fun, Args).

join(Result) -> Result.

-else.
%% FIXME?: These could actually use constrained parallelism via epocxy;
%%  However for now it should be somewhat constrained due to the fact that
%%  all events are processed in sequence via processors. Its fairly
%%  deterministic how many potential threads will be open: i.e.
%%           ( # of Stacks ) X ( # of processors running them )

new( Fun, Args ) when is_list(Args) andalso is_function(Fun,length(Args)) ->
   spawn( wrap_to_hang_for_join( Fun, Args ) ).

join( Future ) ->
   _ = notify_for_join( Future ),
   hang_for_join().
-endif.

%%% ==========================================================================
%%% Internal Functionality
%%% ==========================================================================

%% @hidden
%% @doc Wrap function application with closure and messaging system to send
%%   results back to joiner.
%% @end
wrap_to_hang_for_join( Fun, Args ) ->
  fun() -> future_hanger( catch erlang:apply( Fun, Args ) ) end.

%% @hidden
%% @doc Hang for a message from the joiner so that we can send it our result.
future_hanger( Result ) -> receive {joiner,Pid} -> Pid!{future,Result} end.

%% @hidden
%% @doc Send the Future the address to the local mailbox for results.
notify_for_join( Future ) -> Future ! {joiner,self()}.

%% @hidden
%% @doc Wait for the future to complete so that we get the results back.
hang_for_join() -> receive {future,Result} -> Result end.

