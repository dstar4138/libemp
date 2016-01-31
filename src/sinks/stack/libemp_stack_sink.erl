%%% LibEMP Sink Stack -
%%%
%%%     A Stack of Sinks which can run an event through a list of Sinks
%%%     according to a retry/failure logic setting. Each Sink is wrapped
%%%     with a callback which can either trigger a retry, propagate an
%%%     updated event processing failure, or break the sink stack
%%%     evaluation.
%%%
%%%     It is implemented using the Sink behaviour so as to abstract the
%%%     wiring functionality away from the core of LibEMP. While this is
%%%     a privileged form of sinks, this can be ignored and the developer
%%%     can use raw sinks with processors. Additionally with this
%%%     abstraction, one can have stacks of stacks. With this
%%%     functionality a stack will always allow for the next stack to
%%%     view the event unchanged, it is only within a stack where
%%%     modifications/drops are allowed. Thus Stacks allow for parallel
%%%     computation within a processor itself.
%%%
-module(libemp_stack_sink).
-behaviour(libemp_sink).

-export([
    setup/1, destroy/2,
    validate_configs/1,
    process/3
]).
-export([get_stack/1]).

-include("sink_stack.hrl").
-type libemp_sink_stack() :: libemp_sink_stack:libemp_sink_stack().
-type fault_handler() :: libemp_sink_stack:fault_handler().

%%%===================================================================
%%% LibEMP Sink Behaviour
%%%===================================================================

%% @doc Set up the stack and all sub-sinks.
setup( Args ) ->
  {StackConfig, HandlerFun} = parse_configs( Args ),
  NewState = #stack_state{handler = HandlerFun},
  NewStack = libemp_stack:new( HandlerFun ),
  build_stack( StackConfig, NewStack, NewState ).

%% @doc Loops through all Sinks in the stack and destroys them safely.
destroy( Reason, #stack_state{stack = Stack} ) ->
  libemp_stack:destroy( Reason, Stack ).

%% @doc Check that the configuration for the whole stack is valid.
validate_configs( Args ) ->
  {StackConfigs, HandlerFun} = parse_configs( Args ),
  case validate_handler_fun( HandlerFun ) of
    true  -> validate_stack_configs( StackConfigs );
    Err -> Err
  end.

%% @doc Process an incoming event by running it through the internal sinks.
process( Event, BufferRef, #stack_state{stack=Stack}=S ) ->
  NewStack = libemp_stack_exec:run( Event, BufferRef, Stack ),
  {next, S#stack_state{stack = NewStack}}.

%%%===================================================================
%%% LibEMP Private functionality
%%%===================================================================

%% @private
%% @doc Pull out stack from Sink for recursion purposes.
-spec get_stack( State :: #stack_state{} ) -> libemp_sink_stack().
get_stack( #stack_state{stack=Stack} ) -> Stack.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%% @doc Build a stack object from the StackConfigs.
build_stack( [], Stack, State) ->
  {ok, State#stack_state{stack = Stack}};
build_stack( [Item|Rest], Stack, State ) ->
  case startup(Item, Stack) of
    {ok, NewStack} ->
      build_stack( Rest, NewStack, State );
    {error,Reason} ->
      libemp_stack:destroy({setup, Item, Reason}, Stack)
  end.

%% @hidden
%% @doc Start up the new sink, and add it to the stack based on return.
startup( {SinkModName, Args}, Stack ) ->
  case libemp_sink:setup( SinkModName, Args ) of
    {ok, Sink} ->
      Handler = libemp_stack:get_handler( Stack ),
      {ok, libemp_stack:append(SinkModName,Sink,Handler,Stack)};
    ignore -> {ok, Stack};
    {stop, Reason} -> {error, Reason}
  end;
startup( {SinkModName, Args, FaultHandler}, Stack ) ->
  case libemp_sink:setup(SinkModName, Args) of
    {ok, Sink} ->
      {ok, libemp_stack:append(SinkModName,Sink,FaultHandler,Stack)};
    ignore -> {ok, Stack};
    {stop, Reason} -> {error, Reason}
  end.


%% @hidden
%% @doc This sink expects two parameters, 'stack' and 'fault_function', if
%%   either are missing defaults are provided.
%% @end
parse_configs( Args ) ->
  parse_configs( Args, {?DEFAULT_STACK, ?DEFAULT_FAULT_HANDLER} ).
parse_configs( [], Configs ) ->
  Configs;
parse_configs( [{stack, StackConfig}|Rest], {_, HF}) ->
  parse_configs( Rest, {StackConfig, HF} );
parse_configs( [{fault_function, HandlerFun}|Rest], {SC, _}) ->
  parse_configs( Rest, {SC, HandlerFun} );
parse_configs( [Unknown|Rest], Configs ) ->
  error_logger:info_msg("Stack Config unknown: ~p~n",[Unknown]),
  parse_configs( Rest, Configs ).

%% @hidden
%% @doc Check that the handler function is valid. Only really able to validate
%%   the function exists and its arity is correct.
%% @end
validate_handler_fun( Fun ) when is_function( Fun, 3 ) ->
  libemp_util:function_exists( Fun );
validate_handler_fun( _ ) -> {error, badhandler}.

%% @hidden
%% @doc Check that the configuration for the whole stack is valid.
validate_stack_configs( [] ) -> ok;
validate_stack_configs( [ SinkConnection | Rest ] ) ->
  case check_sink_config( SinkConnection ) of
    ok -> validate_stack_configs( Rest );
    Error -> Error
  end.

%% @hidden
%% @doc Check that the configuration of a single sink is valid.
check_sink_config( {Module, Configs} ) ->
  libemp_sink:validate_configs( Module, Configs ).

