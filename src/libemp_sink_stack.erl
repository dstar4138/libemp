%%% LibEMP Sink Stack - 
%%%
%%%     A Stack of Sinks which can run an event through a list of Sinks
%%%     according to a retry/failure logic setting. Each Sink is wrapped
%%%     with a callback which can either trigger a retry, propogate an
%%%     updated event processing failure, or break the sink stack 
%%%     evaluation. 
%%%
-module(libemp_sink_stack).
-export([new/1, new/2]).
-export([run/3]).
-export([format/1]).
-export([prepend/2,append/2]).
-export([size/1,sinks/1]).
-export([pop/1,drop/1,remove/2]).

-include("internal.hrl").

%% Note the fault handler is only called
-type fault_handler() :: 
        fun( ( CurrentRetryCount :: non_neg_integer(),
               OriginalEvent :: libemp_event(), 
               ExpOrErrorReason :: term() ) 
                    -> fault_handler_return() ).
-type fault_handler_return() :: 
        {ok, libemp_event()} |
        {retry, Count :: pos_integer(), Timeout :: pos_integer()} |
        {error, Reason :: term()}.
%% Default fault handling is to abort execution and let the processor log it.
-define(DEFAULT_FAULT_HANDLER, fun(_,_,Reason) -> {error, Reason} end).
-export_type([fault_handler/0]).

%% A Sink Stack is a set of sinks (ModuleName, State, FaultHandlerFun).
-type libemp_sink_stack() :: [ {atom(), term(), fault_handler()} ].

%% @doc Generates a Sink Stack with the default fault-handling (throwing
%%   the error into LibEMP and aborting the evaluation of subsequent 
%%   sinks), unless given one for that individual Sink.
%% @end 
-spec new( [ Sink ] ) -> {ok, libemp_sink_stack()} | 
                         {error, Reason :: term()}
        when Sink :: {ModuleName :: atom(), State :: term()} |
                     {ModuleName :: atom(), State :: term(), fault_handler()}.
new( OrderedStackList ) ->
    new( OrderedStackList, ?DEFAULT_FAULT_HANDLER ).

%% @doc Generates a Sink Stack with the given fault handler for all sinks. 
-spec new( [ Sink ], fault_handler() ) -> {ok, libemp_sink_stack()} | 
                                          {error, Reason :: term()}
        when Sink :: {ModuleName :: atom(), State :: term()} | 
                     {ModuleName :: atom(), State :: term(), fault_handler()}.
new( OrderedStackList, DefaultFaultHandler ) ->
    lists:map( fun( Sink ) ->
                       to_stack_item( Sink, DefaultFaultHandler ) 
               end, 
               OrderedStackList ).

%% @doc Add a Sink to the beginning of the Stack. 
-spec prepend( Sink , libemp_sink_stack() ) -> libemp_sink_stack()
        when Sink :: {ModuleName :: atom(), State :: term()} |
                     {ModuleName :: atom(), State :: term(), fault_handler()}.
prepend( Sink, SinkStack ) -> 
    [ to_stack_item(Sink, ?DEFAULT_FAULT_HANDLER) | SinkStack ].

%% @doc Add a Sink to the end of the Stack. 
-spec append( Sink , libemp_sink_stack() ) -> libemp_sink_stack()
        when Sink :: {ModuleName :: atom(), State :: term()} |
                     {ModuleName :: atom(), State :: term(), fault_handler()}.
append( Sink, SinkStack ) -> 
    SinkStack ++ [ to_stack_item(Sink, ?DEFAULT_FAULT_HANDLER) ].

%% @doc Get the Sink Module Names in the order they handle events.
-spec sinks( libemp_sink_stack() ) -> [ atom() ].
sinks( SinkStack ) ->
    {ModuleNames, _States, _FaultHandlers} = lists:unzip3( SinkStack ),
    ModuleNames.

%% @doc Get the size of the Sink Stack.
-spec size( libemp_sink_stack() ) -> non_neg_integer().
size( SinkStack) -> length( SinkStack ).

%% @doc Remove the first Sink from the Stack, it will no longer process
%%  anything. This will NOT DESTROY THE SINK, THIS IS UP TO THE CALLER.
%%  This is to allow for reordering/moving to another Stack.
%% @end
-spec pop( libemp_sink_stack() ) -> { Sink | empty, libemp_sink_stack() }
         when Sink :: {ModuleName :: atom(), State :: term(), fault_handler()}.
pop( [] ) -> {empty, []};
pop( [Sink|Stack] ) -> {Sink,Stack}.

%% @doc Remove the last Sink from the Stack, it will no longer process
%%   anything. This will NOT DESTROY THE SINK, THIS IS UP TO THE CALLER.
%%   This is to allow for reordering/moving to another Stack.
%% @end
-spec drop( libemp_sink_stack() ) -> { Sink | empty, libemp_sink_stack() }
         when Sink :: {ModuleName :: atom(), State :: term(), fault_handler()}.
drop( [] ) -> {empty, []};
drop( Stack ) ->
    [Last|Rest] = lists:reverse( Stack ),
    {Last,lists:reverse(Rest)}.

%% @doc Remove a Sink by index in the Stack, it will no longer process
%%  anything. This will NOT DESTROY THE SINK, THIS IS UP TO THE CALLER.
%%   This is to allow for reordering/moving to another Stack.
%% @end
-spec remove( non_neg_integer(), libemp_sink_stack() ) -> 
                                { Sink | empty | badarg, libemp_sink_stack() }
         when Sink :: {ModuleName :: atom(), State :: term(), fault_handler()}.
remove( _Index, [] ) -> {empty,[]};
remove( Index, Stack ) ->
    Indexes = lists:seq( 0, libemp_sink_stack:size( Stack ) ),
    Enumerated  = lists:zip( Indexes, Stack ),
    case lists:keytake( Index, 1, Enumerated ) of
        false -> {badarg, Stack};
        {value, {Index,Sink}, NewEnumerated} ->
            {_Unenumerated, NewStack} = lists:unzip( NewEnumerated ),
            {Sink, NewStack}
    end.

%% @doc Run an Event through a stack of LibEMP Event Sinks. They will
%%   decide at runtime whether to drop the Event or pass it further 
%%   down the line.
%% @end
-spec run( libemp_event(), libemp_buffer:libemp_buffer(), libemp_sink_stack() ) ->
    {error, Reason :: any()} | %TODO: clean up use of any()
    {ok, UpdatedStack :: libemp_sink_stack()}.
run( Event, BufferRef, Stack ) -> run( Event, BufferRef, Stack, [] ).
run( _Event, _BufferRef, [], RStack ) -> lists:reverse( RStack );
run( Event, BufferRef, [Item|Stack], RStack ) -> 
    case run_stack_item( Event, BufferRef, Item ) of
        {error,_}=E -> E;
        {next, NewEvent, NewItem} ->
            run_stack_item( NewEvent, BufferRef, Stack, [NewItem|RStack] )
    end.

%% @doc Pretty-Print format of the Event stack. Ignores printing state and 
%%   fault-handler information by default.
%% @end
-spec format( libemp_sink_stack() ) -> io_lib:chars().
format( Stack ) -> 
    io_lib:format("~p~n",[sinks(Stack)]). %TODO: better pretty print to show order


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%% @doc Process an event using an Item in a Sink Stack (i.e. a Sink). If the
%%   Sink fails, run the Fault Handling function for that sink. This helps with
%%   customization of Stack fault handling. 
%% @end
run_stack_item( Event, BufferRef, SItem ) ->
    RetryCount = 0,
    run_stack_item( Event, BufferRef, SItem, RetryCount ).
run_stack_item( Event, BufferRef, {Name, State, Handler}=SItem, RetryCount ) ->
    case 
        catch erlang:apply( Name, process, [Event, BufferRef, State] )
    of
        % Handle Potential process/3 return values.
        next -> {next, Event, SItem};
        {next, NS} -> {next, Event, {Name, NS, Handler}};
        {next, NE, NS} -> {next, NE, {Name, NS, Handler}};
        drop -> {drop, SItem};
        {drop, NS} -> {drop, {Name, NS, Handler}};

        % Handle the error cases.
        {'EXIT', Reason} ->
            handle_fault( Reason, RetryCount, Handler, Event, BufferRef, SItem )
    end.

%% @hidden
%% @doc Handle the reason for an error by running a Fault Handler. The Fault
%%   handler can also take into account the retry count, and event it failed
%%   on, to return the Timeout (how long to wait before retrying).
%% @end 
handle_fault( Reason, RetryCount, FaultHandler, Event, BufferRef, SItem ) ->
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
                    {error, {too_many_retries, Reason}}
            end;
        {error, NewReason} -> 
            {error, NewReason}
    end.

%% @hidden
%% @doc Convert a tuple of Module name and fault handler or just a Module name
%%   into a Sink Stack item. Presently this is just the same type of tuple,
%%   injecting with the default fault handler if one is not provided.
%% @end
to_stack_item( {ModuleName, State, FaultHandler}, _DefaultHandler ) -> 
    {ModuleName, State, FaultHandler};
to_stack_item( {ModuleName, State}, DefaultHandler ) -> 
    {ModuleName, State, DefaultHandler}.

