%%% LibEMP Sink Behaviour and Object Wrapper - 
%%%
%%%     This module defines two things; the Event Sink Behaviour and the Sink
%%%     Object Wrapper. The Behaviour defined expects only a single function 
%%%     for the processing of events as they come. It is up to the wiring 
%%%     expressed by configuration separately which determines how the Sink is
%%%     stacked with the rest.
%%%
-module(libemp_sink).
-compile(inline).
-include("../internal.hrl").

-export([start/2,stop/2]).
-export([process/3]).
-export([is_pure/1]).

%%% State Initialization: 
%%%   Optional callbacks for state initialization; i.e. if the sink is a pure
%%%   function, it may require no startup or teardown. However, if you require
%%%   a particular module loaded, you can verify and still remain pure (just
%%%   return 'ok').
-optional_callbacks([setup/1, destroy/2]).

%%% Initialize your sink.
-callback setup( Args :: term() ) -> 
    ok |                        % Setup successful and the sink is pure.
    {ok, State :: term()} |     % Setup successful and the sink requires state.
    {stop, Reason :: term()} |  % Setup failed, return an Error.
    ignore.                     % Setup failed, become a passthrough if stacked.

%%% Shutdown your sink.
-callback destroy( Reason :: (normal |             % LibEMP shutdown
                              {shutdown, term()} | % System failure
                              term()),             % Unknown error
                    State :: term() ) -> 
    term(). % Return ignored.

%%% Required Behaviour for all Sinks:

%%% Process the events from the attached buffer. 
%%%   Sinks can be stacked in a synchronous form; they can affect downstream 
%%%   sinks by altering the event that was generated and can even convert it
%%%   into another one. Additionally, it can go ahead and drop the event before
%%%   the rest of the stack can process it. Great for pre-pruning datasets. 
-callback process( Event :: libemp_event(), 
                   BufferRef :: term(), 
                   State :: term() ) -> 
    next |
    {next, NewState :: term()} |
    {next, NewEvent :: libemp_event(), NewState :: term()} |
    drop |
    {drop, NewState :: term()}.


%% LibEMP Sink Object:
%%  This record defines the passed around Sink object which can be compared and
%%  accessed via this module. Any updates to the API would require a change to
%%  this record structure. It can be built using the libemp_sink:start/2
%%  function from inside the sink implementation module.
-record(libemp_sink, {
            module      :: module(), % The callback implementation module. 
            state       :: term(),   % What is the state of the sink. 
            pure = true :: boolean() % Can we make an assumption of purity.
        }).
-type libemp_sink() :: #libemp_sink{}.
-export_type([libemp_sink/0]).


%% @doc Start up the Sink and return the LibEMP Sink object if successful.
-spec start( Module :: module(), State :: term() ) -> 
    {ok, libemp_sink()} | {stop, Reason :: term()} | ignore.
start( Module, Args ) ->
    case 
        erlang:function_exported( Module, setup, 1 )
    of
        true  -> do_setup( Module, Args );
        false -> {ok, #libemp_sink{module=Module, state=[]}}
    end.

%% @doc Stop a Sink, if it is pure, this does nothing.
-spec stop( term(), libemp_sink() ) -> term().
stop( _, #libemp_sink{state=[],pure=true} ) -> 
    ok;
stop( Reason, #libemp_sink{module=Module,state=State} ) ->
    case
        erlang:function_exported( Module, destroy, 2 )
    of
        true -> do_destroy( Reason, Module, State );
        _ -> ok
    end.

%% @doc Run a single event process and return the new Sink closure and the
%%   event after the Sink was able to modify if need be.
%% @end
-spec process( libemp_event(), libemp_buffer:libemp_buffer(), libemp_sink() ) -> 
    {libemp_event(), libemp_sink()}. 
process( Event, Buffer, #libemp_sink{module=Module, state=State}=Sink ) ->
    case do_process( Event, Buffer, Module, State ) of
        next ->
            {next, Event, Sink};
        {next,NewState} -> 
            {next, Event, Sink#libemp_sink{state=NewState}}; 
        {next,NewEvent,NewState} ->
            {next, NewEvent, Sink#libemp_sink{state=NewState}};
        drop ->
            {drop, Sink};
        {drop,NewState} ->
            {drop, Sink#libemp_sink{state=NewState}}
    end.

%% @doc If the sink is pure, this returns true. Purity in this case means that
%%   it does not rely on state. This means we can reschedule and run sinks in
%%   parallel (and generally optimize more when possible).
%% @end 
is_pure( #libemp_sink{pure=true} ) -> true;
is_pure( _ ) -> false.


%%% ==========================================================================
%%% Internal Functionality
%%% ==========================================================================

%% @hidden
%% @doc Run the setup callback and wrap the result in a sink object if
%%  if neccessary.
%% @end
do_setup( Module, Args ) ->
    case 
        catch erlang:apply( Module, setup, [Args] )
    of
        ok -> 
            {ok, #libemp_sink{module=Module, state=[]}};
        {ok, State} -> 
            {ok, #libemp_sink{module=Module, state=State, pure=false}};
        Other -> 
            Other
    end.

%% @hidden
%% @doc Run the destroy callback.
do_destroy( Reason, Module, State ) ->
    catch erlang:apply( Module, destroy, [Reason, State] ).

%% @hidden
%% @doc Run the process callback given an event/buffer pair and the state
%%   of the current module.
%% @end 
do_process( Event, Buffer, Module, State ) ->
    catch erlang:apply( Module, process, [Event, Buffer, State] ). 

