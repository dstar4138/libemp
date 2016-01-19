%%% LibEMP Processor -
%%%
%%%   Processors wrap around the Event Sink so as to provide the wrapper
%%%   coordination of Buffer wiring and Error handling. Multiple processors
%%%   can be wired to the same Buffer. This allows for extreme parallel
%%%   processing as long as the Sinks they wrap have been written correctly.
%%%
%%%   Note however, that if each processor has a different Sink, the
%%%   possibility that the Sink misses an event it is expecting increases.
%%%   Other mechanisms to provide parallelism is to give the Processor a
%%%   Stack Sink, and have that worry about whether it. That way you can have
%%%   serialization with as much parallelism as your system implicitly allows.
%%%
%%%   Note: The Processor does not follow standard `gen_server' behaviour,
%%%   instead it uses the stdlib `gen' module to standardize its looping.
%%%   See `gen_event' as this was modeled after that (except instead of being
%%%   a 'push' based event loop, it is a 'pull' one). This is used to compensate
%%%   for any deviation from the Buffer functionality.
%%%
-module(libemp_processor).

%% API
-export([start_link/3]).
-export([push/2]).

%% Private 'gen' API.
-export([init_it/6, terminate/5]).

-include("internal.hrl").
-define(NO_CALLBACK, 'no callback module').

%%%===================================================================
%%% Internal API
%%%===================================================================

%% @doc Starts the processor server and links it to the buffer.
-spec start_link( atom(), atom(), [term()] ) ->
                      {ok, pid()} | ignore | {error, term()}.
start_link( BufferName, SinkModule, SinkConfigs ) ->
  gen:start( ?MODULE, link, ?NO_CALLBACK, [
      BufferName, SinkModule, SinkConfigs
    ], [] ).

%% @doc Push a set of events to the processor to work on. This should be done
%%   only for debugging or by the Buffer implementation itself if push
%%   processing is necessary. If the processor is already working on a set of
%%   events, these will be effectively appended to the end. If the Processor
%%   is blocked on a buffer's implementation of a `take' this will NOT BYPASS,
%%   instead it will wait until the take completes and then append to the
%%   working set.
%% @end
-spec push( [ libemp_event() ], libemp_processor() ) -> ok.
push( Events, ProcessorRef ) ->
  ProcessorRef ! {events, Events},
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
init_it( Starter, Parent, _, _, Args, Options ) ->
  process_flag( trap_exit, true ),
  Debug = gen:debug_options( Options ),
  case
      catch init_state( Args )
  of
    {ok, TakeBuff, GiveBuff, Sink} ->
      proc_lib:init_ack( Starter, {ok, self()} ),
      loop( Parent, TakeBuff, GiveBuff, Sink, Debug );

    {'EXIT', Reason} ->
      proc_lib:init_ack( Starter, {error, Reason} ),
      exit(Reason);
    Else ->
      proc_lib:init_ack( Starter, {error, Else} ),
      exit(Else)
  end.

%% @private
%% @doc Terminate the pull server. This also has the effect of shutting down the
%%    embedded Sink and unregistering it from the buffer.
%% @end
terminate( Reason, TakeBufferRef, GiveBufferRef, SinkRef, _Debug ) ->
  libemp_buffer:unregister( TakeBufferRef ),
  libemp_buffer:unregister( GiveBufferRef ),
  libemp_sink:destroy( Reason, SinkRef ),
  exit( Reason ).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%% @doc Initialize the state by pulling from application configuration.
init_state([ BufferName, SinkModule, SinkConfigs ]) ->
  {ok, BufferTakeRef} = libemp_buffer:register( take, BufferName ),
  {ok, BufferGiveRef} = libemp_buffer:register( give, BufferName ),
  case libemp_sink:setup( SinkModule, SinkConfigs ) of
    {ok, SinkRef} ->
      {ok, BufferTakeRef, BufferGiveRef, SinkRef};
    Err ->
      libemp_buffer:unregister( BufferTakeRef ),
      libemp_buffer:unregister( BufferGiveRef ),
      Err
  end.

%% @hidden
%% @doc Run the processing loop. Listen for events being pushed or pull the
%%   next batch from the Buffer.
%% @end
loop( Parent, TakeBuffer, GiveBuffer, Sink, DebugOpts ) ->
  receive
    {events, Events} ->
        NewSink = process( Events, GiveBuffer, Sink, DebugOpts ),
        loop( Parent, TakeBuffer, GiveBuffer, NewSink, DebugOpts );

    shutdown -> terminate( shutdown, TakeBuffer, GiveBuffer, Sink, DebugOpts );
    Unknown  -> ?ERROR("Unknown Message to libemp_processor: ~p~n",[Unknown])
  after 0 ->
    Events = libemp_buffer:take( TakeBuffer ),
    NewSink = process( Events, GiveBuffer, Sink, DebugOpts ),
    loop( Parent, TakeBuffer, GiveBuffer, NewSink, DebugOpts )
  end.

%% @hidden
%% @doc Perform the batch event processing by folding the Sink state over
%%    each event in order.
%% @end
process( Events, Buffer, Sink, _Debug ) ->
  lists:foldl( fun(Event, SinkRef) ->
                  %TODO: Do some debuggery here if needed.
                  to_sink(libemp_sink:process( Event, Buffer, SinkRef ))
               end, Sink, Events ).

%% @hidden
%% @doc Get the sink from the libemp_sink:process/3 return value.
to_sink( {next,_,Sink} ) -> Sink;
to_sink( {drop,  Sink} ) -> Sink.
