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
-module(libemp_processor).
-behaviour(gen_server).

-include("internal.hrl").

%% API
-export([start_link/3]).
-export([push/2]).
-export([stop/1,stop/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%% Private API
-export([taker_loop/2]).

-define(SERVER, ?MODULE).

-record(state, {
  taker_pid, take_buf, give_buf, sink_ref
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the processor server and links it to the buffer.
-spec start_link( atom(), atom(), [term()]) ->
  {ok, Pid :: pid()} | {error, Reason :: term()}.
start_link( BufferName, SinkModule, SinkConfigs ) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [
    BufferName, SinkModule, SinkConfigs
  ], []).

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
  gen_server:cast( ProcessorRef, {events, Events} ).

%% @private
%% @doc These should not be called by hand. This will force a stop of the
%%   processor, but if it is part of the supervision tree, it will get
%%   restarted. Use libemp_processor_sup:remove_processor/1.
%% @end
stop( ProcessorRef ) ->
  gen_server:stop( ProcessorRef ).
stop( Reason, ProcessorRef ) ->
  gen_server:stop( ProcessorRef, Reason, infinity ).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server state.
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(Args) ->
  process_flag( trap_exit, true ),
  {ok, State} = init_state( Args ),
  {ok, TakerPid} = start_taker( State ),
  {ok, State#state{taker_pid = TakerPid}}.

%% @private
%% @doc Handling call messages
handle_call(_Request, _From, State) -> {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}.
handle_cast({events, Events}, State) ->
  % Halt the Processor while it does its thing.
  NewState = process( Events, State ),
  ready_the_taker( NewState ),
  {noreply, NewState}.

%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}.
%TODO: handle taker failures.
handle_info(_Info, State) -> {noreply, State}.

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term().
terminate( Reason, State ) -> do_terminate( Reason, State ).

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) -> {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_state([ BufferName, SinkModule, SinkConfigs ]) ->
  {ok, BufferTakeRef} = libemp_buffer:register( take, BufferName ),
  {ok, BufferGiveRef} = libemp_buffer:register( give, BufferName ),
  case libemp_sink:setup( SinkModule, SinkConfigs ) of
    {ok, SinkRef} ->
      libemp_node:save_default_proc( BufferTakeRef, self() ),
      {ok, #state{
        take_buf = BufferTakeRef,
        give_buf = BufferGiveRef,
        sink_ref = SinkRef
      }};
    Err ->
      libemp_buffer:unregister( BufferTakeRef ),
      libemp_buffer:unregister( BufferGiveRef ),
      Err
  end.

start_taker( State ) ->
  Pid = spawn_link( ?MODULE, taker_loop, [self(), State#state.take_buf] ),
  {ok, Pid}.
taker_loop( Processor, Taker ) ->
  Events = libemp_buffer:take( Taker ),
  libemp_processor:push( Events, Processor ),
  taker_wait_for_ready( Processor, Taker ).
taker_wait_for_ready( Processor, Taker ) ->
  receive
    ready -> taker_loop( Processor, Taker );
    Reason -> exit(Reason)
  end.
ready_the_taker( #state{taker_pid = Taker} ) ->
  Taker ! ready.
brute_kill_taker( Taker ) ->
  exit( Taker, shutdown ).

do_terminate( Reason, #state{taker_pid = Taker} = State ) ->
  brute_kill_taker( Taker ),
  libemp_node:remove_default_proc( State#state.take_buf, self() ),
  libemp_buffer:unregister( State#state.take_buf ),
  libemp_buffer:unregister( State#state.give_buf ),
  libemp_sink:destroy( Reason, State#state.sink_ref ),
  ok.

%% @hidden
%% @doc Perform the batch event processing by folding the Sink state over
%%    each event in order.
%% @end
process( Events, State ) ->
  Buffer = State#state.give_buf,
  Sink = State#state.sink_ref,
  NewSink = lists:foldl( fun(Event, SinkRef) ->
    %TODO: Do some debuggery here if needed.
    to_sink(libemp_sink:process( Event, Buffer, SinkRef ))
  end, Sink, Events ),
  State#state{sink_ref = NewSink}.

%% @hidden
%% @doc Get the sink from the libemp_sink:process/3 return value.
to_sink( {next,_,Sink} ) -> Sink;
to_sink( {drop,  Sink} ) -> Sink.