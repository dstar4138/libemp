%%% Libemp Process Signal Trapper Service -
%%%
%%%     Uses Erlang's built in Process monitoring capabilities to monitor a
%%%     service for outage.
%%%
-module(libemp_trapper).
-behaviour(gen_server).

%% API
-export([start_link/2]).
-export([stop/1, stop/2]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(SERVER, ?MODULE).
-record(state, { pid, terminate_cb, failure_cb }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the processor server and links it to the buffer.
-spec start_link( pid(), #{ atom() => fun() } ) ->
  {ok, Pid :: pid()} | {error, Reason :: term()}.
start_link( Pid, Callbacks ) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [
      Pid, Callbacks
  ], []).

%% @doc Stop the monitoring service at the specific process id.
stop( Pid ) -> gen_server:cast( Pid, shutdown ).
stop( Reason, Pid ) -> gen_server:cast( Pid, {shutdown, Reason} ).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server state.
init(Args) ->
  process_flag( trap_exit, true ),
  {ok, build_state( Args )}.

%% @private
%% @doc Handling API Messages. This is unused presently.
handle_cast(shutdown, State) -> {stop, shutdown, State};
handle_cast({shutdown, _Reason}=Res, State) -> {stop, Res, State};
handle_cast(_Request, State) -> {noreply, State}.
handle_call(_Request, _From, State) -> {reply, {error, badarg}, State}.

%% @private
%% @doc Handling all non call/cast messages
handle_info({'EXIT', _Pid, Reason}, State) ->
  {stop, Reason, State};
handle_info({'DOWN', _Ref, process, _Pid, Reason}, State) ->
  {stop, Reason, State};
handle_info(_Info, State) -> {noreply, State}.

%% @private
%% @doc Handle termination and failure callbacks on the monitored service.
terminate( shutdown, #state{terminate_cb = Callback} ) ->
  catch Callback(), ok;
terminate( Reason, #state{failure_cb = Callback} ) ->
  catch Callback(Reason), ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%% ==========================================================================
%%% Internal Functionality
%%% ==========================================================================

%% @hidden
%% @doc Force monitoring of the process id, and then save the callbacks.
build_state( [Pid, Callbacks] ) ->
  erlang:monitor( process, Pid ),
  #state{
    pid = Pid,
    terminate_cb = get_cb( terminate, 0, Callbacks ),
    failure_cb   = get_cb( failure, 1, Callbacks )
  }.
get_cb( Name, Arity, Callbacks ) ->
  maps:get( Name, Callbacks, fake_fun( Arity ) ).
fake_fun( 0 ) -> fun()  -> ok end;
fake_fun( 1 ) -> fun(_) -> ok end.
