%%% LibEMP Sink Stack Execution -
%%%
%%%   This module defines how we should be executing a stack of sinks.
%%%
-module(libemp_stack_exec).

-export([run/3]).

-include("sink_stack.hrl").
-type libemp_sink_stack() :: libemp_sink_stack:libemp_sink_stack().
-type fault_handler() :: libemp_sink_stack:fault_handler().

%% @doc Abstract over the stack fold to run the event over the Sink Stack.
run( Event, Buffer, Stack ) ->
  StackList = libemp_stack:to_list( Stack ),
  NewStackList = run_stack( Event, Buffer, StackList ),
  libemp_stack:from_list( NewStackList, Stack ).

%%%===================================================================
%%% Stack Execution
%%%===================================================================

run_stack( Event, Buffer, Stack ) ->
  State = {ok, [], []}, % { status, [ stack_item ], [ future ] }
  case
    lists:foldl( do_processing(Event,Buffer), State, Stack )
  of
    {ok, [], Futures} -> hang_for_futures( Futures, [] );
    {wait, WaitQueue, [NonStackItem|Futures]} ->
      RightSideWaitQueue = lists:reverse(WaitQueue),
      NewStackTail = case hang_for_future( NonStackItem ) of
        {next, NewEvent, StackItem} ->
          [StackItem|run_stack( NewEvent, Buffer, RightSideWaitQueue )];
        {drop, StackItem} -> [StackItem|RightSideWaitQueue];
        {error,_Reason,StackItem} -> [StackItem|RightSideWaitQueue]
      end,
      hang_for_futures( Futures, [] ) ++ NewStackTail
  end.

do_processing( Event, Buffer ) ->
  fun ( StackItem, {wait, WaitQueue, CurrentProcessed} ) ->
            {wait, [StackItem|WaitQueue], CurrentProcessed };
      ( StackItem, {ok, WaitQueue, CurrentProcessed}) ->
            case is_stack(StackItem) of
              true ->
                % If it is a queue, and we are ok to process, run the future.
                Future = start_processing( Event, Buffer, StackItem ),
                {ok, WaitQueue, [Future|CurrentProcessed]};
              false ->
                % If it is not, then create the future, and force list serial.
                Future = start_processing( Event, Buffer, StackItem ),
                {wait, WaitQueue, [Future|CurrentProcessed]}
            end
  end.

is_stack( Sink ) -> libemp_stack:item_get_module(Sink) =:= libemp_stack_sink.
start_processing( Event, Buffer, StackItem ) ->
  libemp_stack_future:new(fun run_stack_item/3, [Event,Buffer,StackItem]).
hang_for_future( Future ) -> libemp_stack_future:join( Future ).
hang_for_futures( [], Items ) -> Items;
hang_for_futures( [Future|Rest], Items ) ->
  StackItem = ignore_result_for_item( libemp_stack_future:join(Future) ),
  hang_for_futures( Rest, [StackItem|Items] ).
ignore_result_for_item( {next,_NewEvent,Item} ) -> Item;
ignore_result_for_item( {drop,Item} ) -> Item;
ignore_result_for_item( {error,_Reason,Item}) -> Item. %TODO: log err reason.

%% @hidden
%% @doc Process an event using an Item in a Sink Stack (i.e. a Sink). If the
%%   Sink fails, run the Fault Handling function for that sink. This helps with
%%   customization of Stack fault handling. On Sub-Stacks, we attempt to run it
%%   in parallel as the
%% @end
run_stack_item( Event, BufferRef, SItem ) ->
  RetryCount = 0,
  run_stack_item( Event, BufferRef, SItem, RetryCount ).
run_stack_item( Event, BufferRef, SItem, RetryCount ) ->
  Sink = libemp_stack:item_get_sink( SItem ),
  case libemp_sink:process( Event, BufferRef, Sink ) of
  % Handle Potential process/3 return values.
    {next, NE, NS} -> {next, NE, libemp_stack:item_set_sink(NS, SItem)};
    {drop, NS}     -> {drop, libemp_stack:item_set_sink(NS,SItem)};

  % Ignore match errors (i.e. sink doesn't expect/care about that event).
  % Examples:
  %   X = fun(2) -> ok end, X(test).
  %   Y = fun(Z) when is_integer(Z) -> ok end, Y(test).
    {'EXIT', {function_clause, _}} -> {next, Event, SItem};

  % Handle the other error cases.
    {'EXIT', Reason} ->
      handle_fault( Reason, RetryCount, Event, BufferRef, SItem )
  end.

%% @hidden
%% @doc Handle the reason for an error by running a Fault Handler. The Fault
%%   handler can also take into account the retry count, and event it failed
%%   on, to return the Timeout (how long to wait before retrying).
%% @end
handle_fault( Reason, RetryCount, Event, BufferRef, SItem ) ->
  FaultHandler = libemp_stack:item_get_handler( SItem ),
  case
    catch erlang:apply( FaultHandler, [RetryCount, Event, Reason] )
  of
    {ok, E} ->
      {next, E, SItem};
    {retry, Count, Timeout} ->
      case Count >= RetryCount of
        false ->
          timer:sleep( Timeout ),
          run_stack_item( Event, BufferRef, SItem, RetryCount+1 );
        true ->
          {error, {too_many_retries, Reason}, SItem}
      end;
    {error, NewReason} ->
      {error, NewReason, SItem}
  end.

