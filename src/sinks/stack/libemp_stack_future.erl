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

%TODO: Fix these fake futures, they should only be turned on if sequential/debug.
%-ifdef(DEBUG).
new( Fun, Args ) when is_list(Args) andalso is_function(Fun, length(Args)) ->
  erlang:apply(Fun, Args).

join(Result) -> Result.
%-else.
% TODO: These should actually use constrained parallelism via epocxy.
%new( Fun, Args ) when is_list(Args) andalso is_function(Fun,length(Args)) ->
%   Pid = spawn( wrap_to_hang_for_join( Fun, Args ) ),
%   {Pid}.
%
%join( {Pid}=_Future ) ->
%   notify_for_join( Pid ),
%   hang_for_join().
%
%-endif.

%%% ==========================================================================
%%% Internal Functionality
%%% ==========================================================================

%% FIXME?: the following is gross for noisy joiners.
wrap_to_hang_for_join( Fun, Args ) ->
  fun() -> future_hanger( catch erlang:apply( Fun, Args ) ) end.
future_hanger( Result ) -> receive {joiner,Pid} -> Pid!{future,Result} end.
notify_for_join( Pid ) -> Pid!{joiner,self()}.
hang_for_join() -> receive {future,Result} -> Result end.

