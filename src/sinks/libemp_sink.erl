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

-export([setup/2,destroy/2]).
-export([validate_configs/2]).
-export([process/3]).
-export([is_pure/1]).

%%% State Initialization: 
%%%   Optional callbacks for state initialization; i.e. if the sink is a pure
%%%   function, it may require no startup or teardown. However, if you require
%%%   a particular module loaded, you can verify and still remain pure (just
%%%   return 'ok').
-optional_callbacks([setup/1, destroy/2, validate_configs/1]).

%%% Initialize your sink.
-callback setup( Args :: [ term() ] ) ->
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

%%% Validate the configurations before startup.
-callback validate_configs( [ Args::term() ] ) ->
    ok |                        % If all is as expected.
    {error, Reason :: term()}.  % If LibEMP should not startup due to mis-config


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
-opaque libemp_sink() :: #libemp_sink{}.
-export_type([libemp_sink/0]).


%% @doc Start up the Sink and return the LibEMP Sink object if successful.
-spec setup( Module :: module(), State :: term() ) ->
    {ok, libemp_sink()} | {stop, Reason :: term()} | ignore.
setup( Module, Args ) ->
    case 
        libemp_util:function_exists( fun Module:setup/1, true )
    of
        true  -> do_setup( Module, Args );
        false -> {ok, #libemp_sink{module=Module, state=[]}}
    end.

%% @doc Stop a Sink, if it is pure, this does nothing.
-spec destroy( term(), libemp_sink() ) -> term().
destroy( _, #libemp_sink{state=[],pure=true} ) ->
    ok;
destroy( Reason, #libemp_sink{module=Module,state=State} ) ->
    do_destroy( Reason, Module, State ).

%% @doc Run a single event process and return the new Sink closure and the
%%   event after the Sink was able to modify if need be.
%% @end
-spec process( libemp_event(), libemp_buffer:libemp_buffer(), libemp_sink() ) ->
    {next, libemp_event(), libemp_sink()} | {drop, libemp_sink()}.
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

%% @doc Validate the sink configurations given the Module defining the Sink
%%    behaviour, and the Configs which should be validated by the Module.
%% @end
validate_configs( Module, Configs ) when is_atom( Module ) ->
    libemp_util:wrap_extern( Module, validate_configs, [Configs], ok ).

%%% ==========================================================================
%%% Internal Functionality
%%% ==========================================================================

%% @hidden
%% @doc Run the setup callback and wrap the result in a sink object if
%%  if necessary.
%% @end
do_setup( Module, Args ) ->
    Result = (catch libemp_util:wrap_extern( Module, setup, [Args], ok )),
    ?LOG("Sink(~p) Setup: ~p~n",[Module, Result]),
    case Result of
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
    Args = [Reason, State],
    Result = (catch libemp_util:wrap_extern(Module, destroy, Args, ok)),
    ?LOG("Sink(~p) Destroy: ~p~n",[Module, Result]),
    Result.


%% @hidden
%% @doc Run the process callback given an event/buffer pair and the state
%%   of the current module.
%% @end 
do_process( Event, Buffer, Module, State ) ->
    Args = [Event, Buffer, State],
    Result = (catch libemp_util:wrap_extern( Module, process, Args )),
    ?LOG("Sink(~p) Process: ~p~n",[Module, Result]),
    Result.

