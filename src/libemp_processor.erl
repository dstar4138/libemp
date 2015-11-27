%%% LibEMP Event Processor -  
%%%
%%%     The Event Processor wraps a stack of Event Sinks initialized during
%%%     start up and a configuration of how to handle Sink faults. 
%%%
%%%     Note: The Processor does not follow gen_server, instead it uses the
%%%     stdlib `gen` module to standardize its looping functionality, see
%%%     `gen_event` as this was modeled after that (except using a `pull`
%%%     rather than a `push`).
%%%
-module(libemp_processor).

%% API
-export([start_link/3,validate_configs/3,push/2]).

%% Private `gen` API
-export([init_it/6]).
-export([terminate/4]).

%% Private Exports
-export([run_stack/4]).

-include("internal.hrl").

-define(NO_CALLBACK, 'no callback module').

-type libemp_processor() :: pid() | atom().

%%%===================================================================
%%% Internal API
%%%===================================================================

%% @doc Starts the processor server.
-spec start_link( atom(), fun(), [term()] ) -> {ok, pid()} |
                                               ignore | {error, term()}.
start_link( BufferName, HandlerFun, StackConfigs ) ->
    gen:start( ?MODULE, link, ?NO_CALLBACK, [], [
        BufferName, HandlerFun, StackConfigs
    ] ).

%% @doc Validate the stack configurations before starting it up.
-spec validate_configs( atom(), fun(), [term()] ) -> ok | {error, term()}.
validate_configs( BufferName, HandlerFun, StackConfigs ) ->
    %TODO: Check if buffername is running, that HandlerFun meets criteria,
    %  and that each sink in the stack has valid configurations.
    ok.

%% @doc Push a set of events to the processor to work on. This should be
%%   done only for debugging or by the Buffer implementation itself if
%%   push processing is neccessary. If the processor is already working
%%   on a set of events, these will be effectively appended to the end.
%% @end
-spec push( [ libemp_event() ], libemp_processor() ) -> ok.
push( Events, Processor ) ->
    Processor ! {events, Events},
    ok.

%%%===================================================================
%%% Private API
%%%===================================================================

%% @private
%% @doc Initialize the generic pull server. This will loop and pull for
%%   events from a LibEMP Buffer by requesting
%% @end 
init_it( Starter, self, Name, Mod, Args, Options ) ->
    init_it( Starter, self(), Name, Mod, Args, Options ); 
init_it( Starter, Parent, _, _, _, Options ) ->
    process_flag( trap_exit, true ),
    Debug = gen:debug_options( Options ),
    case catch init_state() of
        {ok, Buff, Stack} -> 
            proc_lib:init_ack( Starter, {ok, self()} ),
            loop( Parent, Buff, Stack, Debug );
        {'EXIT', Reason} -> 
            proc_lib:init_ack( Starter, {error, Reason} ),
            exit(Reason);
        Else ->
            proc_lib:init_ack( Starter, {error, Else} ),
            exit(Else)
    end.

%% @private
%% @doc Run an Event through a Sink Stack. It requires a refrence to a
%%   Buffer so as to retrigger events if neccessary.
%% @end
run_stack( Event, BufferRef, Stack, _DebugOptions ) ->
    %TODO: examine DEBUG options to manipulate the run.
    libemp_sink_stack:run( Event, BufferRef, Stack ). 


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%% @doc Initialize the state by pulling from application configuration.
init_state() ->
    {ok, StackConfigs} = get_stack_configs(),
    {ok, Stack} = build_stack( StackConfigs ),
    {ok, Buffer} = determine_buffer_ref(),
    {ok, BufferRef} = libemp_buffer:register( take, Buffer ),
    {ok, BufferRef, Stack}.

%% @hidden
%% @doc Listen for events being pushed, or pull the next batch. 
loop( Parent, Buffer, Stack, Debug ) ->
    receive
        {events, Events} ->
            process( Events, Buffer, Stack, Debug ),
            loop( Parent, Buffer, Stack, Debug );
        shutdown -> 
            terminate( Parent, Buffer, Stack, Debug );
        Unknown ->
            ?ERROR("Unknown Message to libemp_processor: ~p~n",[Unknown])
    after 0 ->
        Events = libemp_buffer:take( Buffer ), 
        NewStack = process( Events, Buffer, Stack, Debug ),
        loop( Parent, Buffer, NewStack, Debug )
    end.

%% @hidden
%% @doc Destroy all the Sinks in the Stack, then unregister from the Buffer,
%%   finally remove self from Parent.
%% @end  
terminate( Parent, Buffer, Stack, Debug ) -> 
    ok. %TODO: contact buffer, stacks and parent due to shutdown.

%% @hidden
%% @doc Process a set of events in order via the Stack of Sinks.
process( Events, BufferRef, Stack, Debug ) ->
   lists:foldl( fun( Event, StackAcc ) ->
                    libemp_processor:run_stack( Event, BufferRef, StackAcc, Debug )
                end, Stack, Events ).

%% @hidden
%% @doc Get the configurations for the stack this processor will contain.
get_stack_configs() -> 
    %TODO: Get the configuration. 
    {ok,[]}.

%% @hidden
%% @doc Build the set of Sinks into a Stack.
build_stack( _Configs ) -> 
    %TODO: Build each sink and add it to a stack.
    {ok, []}.

%% @hidden
%% @doc Pulling Buffer reference out of Node's State Server. Will also register
%%   with the buffer as a consumer.
%% @end
determine_buffer_ref() ->
    %TODO: wire up to specific buffer. For now just pulling from top of registry.
    {ok, Buffer} = libemp_node:get_buffer(),
    {ok, _ConsumerSideRef} = libemp_buffer:register( take, Buffer ).

