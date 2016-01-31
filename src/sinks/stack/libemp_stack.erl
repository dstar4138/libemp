%%% LibEMP Sink Stack - 
%%%
%%%     A Stack of Sinks which can run an event through a list of Sinks
%%%     according to a retry/failure logic setting. Each Sink is wrapped
%%%     with a callback which can either trigger a retry, propogate an
%%%     updated event processing failure, or break the sink stack 
%%%     evaluation.
%%%
%%%     It is implemented using the Sink behaviour so as to abstract the
%%%     wiring functionality away from the core of LibEMP. While this is
%%%     a privilaged form of sinks, this can be ignored and the developer
%%%     can use raw sinks with processors. Additionally it is possible to
%%%     wire up stacks of stacks
%%%
-module(libemp_stack).

-export([new/1, new/2]).
-export([destroy/2]).
-export([format/1]).
-export([prepend/2,prepend/4]).
-export([append/2,append/4]).
-export([size/1,sinks/1]).
-export([pop/1,drop/1,remove/2]).
-export([get_handler/1]).
-export([to_list/1,from_list/2]).

-export([item_get_handler/1]).
-export([item_get_module/1]).
-export([item_get_sink/1,item_set_sink/2]).

-include("sink_stack.hrl").

%% Note the fault handler is only called
-opaque fault_handler() :: fun( ( CurrentRetryCount :: non_neg_integer(),
                                OriginalEvent :: libemp_event(),
                                ExpOrErrorReason :: term() )
                                    -> fault_handler_return() ).

-opaque fault_handler_return() ::
        {ok, libemp_event()} |
        {retry, Count :: pos_integer(), Timeout :: pos_integer()} |
        {error, Reason :: term()}.

%% A Sink Stack is a set of sinks (ModuleName, Args, State, FaultHandlerFun).
-record(libemp_stack_item,
            {module, sink, handler=?DEFAULT_FAULT_HANDLER}).
-opaque libemp_stack_item() :: #libemp_stack_item{}.

-record(libemp_sink_stack, {
                stack=[]                       :: [ libemp_stack_item() ],
                handler=?DEFAULT_FAULT_HANDLER :: fault_handler()
}).
-opaque libemp_sink_stack() :: #libemp_sink_stack{}.

-export_type([fault_handler/0, fault_handler_return/0]).
-export_type([libemp_sink_stack/0, libemp_stack_item/0]).

%% Type redefinitions to make it easier to read.
-type libemp_sink() :: libemp_sink:libemp_sink().

%% @doc Generates an empty Sink Stack with the default fault-handling
%%   (throwing the error into LibEMP and aborting the evaluation of
%%   subsequent sinks).
%% @end
-spec new( fault_handler() ) -> libemp_sink_stack().
new( DefaultHandler ) ->
  #libemp_sink_stack{handler = DefaultHandler}.

%% @doc Clone a sink stack given a list of sink items and the handler
%%    to use on faults by default.
%% @end
-spec new( [libemp_stack_item()], fault_handler() ) -> libemp_sink_stack().
new(SinkItems, DefaultHandler) ->
  #libemp_sink_stack{stack = SinkItems, handler = DefaultHandler}.

%% @doc Runs destroy/2 on all sinks in the stack.
destroy( Reason, #libemp_sink_stack{stack=Stack} ) ->
  lists:foreach(fun(#libemp_stack_item{sink = Sink}) ->
    libemp_sink:destroy(Reason, Sink)
  end, Stack ).


%% @doc Add a Sink to the beginning of the Stack. 
-spec prepend( libemp_stack_item(), libemp_sink_stack() ) -> libemp_sink_stack().
prepend( Sink, #libemp_sink_stack{stack=Stack}=S ) ->
  NewStack = [Sink|Stack],
  S#libemp_sink_stack{stack=NewStack}.

%% @doc Construct a Sink and add it to the beginning of the Stack.
-spec prepend( module(), libemp_sink(), fault_handler(), libemp_sink_stack()) ->
          libemp_sink_stack().
prepend( Module, SinkObj, FaultHandler, #libemp_sink_stack{stack=Stack}=S) ->
  NewItem = #libemp_stack_item{ module = Module,
                                sink = SinkObj,
                                handler = FaultHandler },
  NewStack = [ NewItem | Stack ],
  S#libemp_sink_stack{stack=NewStack}.

%% @doc Add a Sink to the end of the Stack.
-spec append( libemp_stack_item(), libemp_sink_stack() ) -> libemp_sink_stack().
append( Sink, #libemp_sink_stack{stack=Stack}=S ) ->
  NewStack = Stack ++ [ Sink ],
  S#libemp_sink_stack{stack=NewStack}.

%% @doc Construct a Sink Item, and add it to the end of the Stack.
-spec append( module(), libemp_sink(), fault_handler(), libemp_sink_stack()) ->
          libemp_sink_stack().
append( Module, SinkObj, FaultHandler, #libemp_sink_stack{stack=Stack}=S) ->
  NewItem = #libemp_stack_item{ module = Module,
                                sink = SinkObj,
                                handler = FaultHandler },
  NewStack = Stack ++ [ NewItem ],
  S#libemp_sink_stack{stack=NewStack}.


%% @doc Get the Sink Module Names in the order they handle events.
-spec sinks( libemp_sink_stack() ) -> [ atom() ].
sinks( #libemp_sink_stack{stack=S} ) ->
  lists:foldl( fun(#libemp_stack_item{module = M}, Ms) -> [M|Ms] end, [], S ).


%% @doc Get the size of the Sink Stack.
-spec size( libemp_sink_stack() ) -> non_neg_integer().
size( #libemp_sink_stack{stack = S}) -> length( S ).

%% @doc Remove the first Sink from the Stack, it will no longer process
%%  anything. This will NOT DESTROY THE SINK, THIS IS UP TO THE CALLER.
%%  This is to allow for reordering/moving to another Stack.
%% @end
-spec pop( libemp_sink_stack() ) ->
                { libemp_stack_item() | empty, libemp_sink_stack() }.
pop( #libemp_sink_stack{stack=[]} ) -> {empty, []};
pop( #libemp_sink_stack{stack=[Sink|Stack]}=S ) ->
  {Sink,S#libemp_sink_stack{stack=Stack}}.

%% @doc Remove the last Sink from the Stack, it will no longer process
%%   anything. This will NOT DESTROY THE SINK, THIS IS UP TO THE CALLER.
%%   This is to allow for reordering/moving to another Stack.
%% @end
-spec drop( libemp_sink_stack() ) ->
                { libemp_stack_item() | empty, libemp_sink_stack() }.
drop( #libemp_sink_stack{stack = []} ) -> {empty, []};
drop( #libemp_sink_stack{stack = Stack} = S ) ->
    [Last|Rest] = lists:reverse( Stack ),
    {Last,S#libemp_sink_stack{stack = lists:reverse(Rest)}}.

%% @doc Remove a Sink by index in the Stack, it will no longer process
%%  anything. This will NOT DESTROY THE SINK, THIS IS UP TO THE CALLER.
%%   This is to allow for reordering/moving to another Stack.
%% @end
-spec remove( non_neg_integer(), libemp_sink_stack() ) -> 
                  { libemp_stack_item() | empty | badarg, libemp_sink_stack() }.
remove( _Index, #libemp_sink_stack{stack = []} ) -> {empty,[]};
remove( Index, #libemp_sink_stack{stack = Stack} = S ) ->
    Indexes = lists:seq( 0, libemp_sink_stack:size( Stack ) ),
    Enumerated  = lists:zip( Indexes, Stack ),
    case lists:keytake( Index, 1, Enumerated ) of
        false -> {badarg, S};
        {value, {Index,Sink}, NewEnumerated} ->
            {_Unenumerated, NewStack} = lists:unzip( NewEnumerated ),
            {Sink, S#libemp_sink_stack{stack = NewStack}}
    end.

to_list( #libemp_sink_stack{stack = Stack} ) -> Stack.
from_list( StackList, #libemp_sink_stack{}=S) -> S#libemp_sink_stack{stack=StackList}.


%% @doc Pretty-Print format of the Event stack. Ignores printing state and
%%   fault-handler information by default.
%% @end
-spec format( libemp_sink_stack() ) -> io_lib:chars().
format( #libemp_sink_stack{stack = Stack} ) ->
    lists:flatten(
      lists:foldl(fun pprint/2, {[],1}, Stack)).

%% @hidden
%% @doc Attempt to pretty print, by recursing on stacks.
pprint(#libemp_stack_item{module = libemp_stack_sink, sink=Sink}, {Buffer,Indent}) ->
{
  [Buffer, lists:fold(fun pprint/2,{[],Indent+1}, libemp_stack_sink:get_stack(Sink)),"~n"],
 Indent
};
pprint(#libemp_stack_item{ module = Sink },{Buffer,Indent}) ->
  {[Buffer,
    " ", %TODO: indent correctly.
  io_lib:format("~p~n",[Sink])], Indent}.

%% @doc Get the default error handler from the Stack.
get_handler( #libemp_sink_stack{handler = Handler} ) -> Handler.

item_get_module( #libemp_stack_item{module = Module} ) -> Module.

item_get_handler( #libemp_stack_item{handler = Handler} ) -> Handler.

item_get_sink(#libemp_stack_item{sink=Sink} ) -> Sink.

item_set_sink(Sink, #libemp_stack_item{}=S) -> S#libemp_stack_item{sink=Sink}.