%%% LibEMP Node Self Monitor -
%%%
%%%   When this Monitor is running, the LibEMP node can publish events into
%%%   an event stream to be handled by implemented sinks, it is effectively
%%%   a wrapper around a buffer that the node can serialize the events.
%%%
-module(libemp_node_monitor).
-behaviour(gen_server).
-behaviour(libemp_monitor).
-include("../../internal.hrl").

%% monitor Api
-export([
  setup/3,
  destroy/3
]).

%% server Api
-export([
  start_link/1,
  emit_log/3,
  emit_app_event/3,
  emit_generic_event/1,
  stop/2
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%%%===================================================================
%%% libemp_monitor API
%%%===================================================================

%% @doc Start the gen_server to wrap around the wired buffer.
setup( _Args, _Configs, EMP ) ->
  libemp_node_monitor:start_link( EMP ).

%% @doc Stop the gen_server.
destroy( Reason, Pid, _EMP ) ->
  libemp_node_monitor:stop( Reason, Pid ).

%%%===================================================================
%%% server API
%%%===================================================================

%% @doc Special node event, a log, these go into the primary node event stream.
emit_log( Level, Location, Message ) ->
  Event = {log, Level, Location, Message},
  gen_server:cast( ?MODULE, {event, Event} ),
  ?LOG( "~p:~p:~s~n", [Level, Location, Message] ).

%% @doc Special node event, an app change (i.e. add/remove buffer/monitor, etc).
emit_app_event( NewOrDelete, Type, Details ) ->
  Event = {node, NewOrDelete, Type, Details},
  gen_server:cast( ?MODULE, {event, Event} ).

%% @doc Send a generic event to the system event stream.
emit_generic_event( Generic ) ->
  Event = {node, generic, Generic},
  gen_server:cast( ?MODULE, {event, Event} ).

%% @private
%% @doc Starts the port server, with the given name.
start_link( EMP ) ->
  gen_server:start_link( {local, ?MODULE}, ?MODULE, [ EMP ], [] ).

%% @private
%% @doc Stop the port server.
stop( Reason, Pid ) ->
  gen_server:cast( Pid, {shutdown, Reason} ).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-record(state, {emp}).

%% @private
%% @doc Initializes the server
init([ EMP ]) -> {ok, #state{emp = EMP}}.

%% @private
%% @doc Handling server api messages.
handle_cast( {event, Cmd}, State ) -> emit( Cmd, State );
handle_cast( {shutdown,_}=Reason, State ) -> {stop, Reason, State};
handle_cast(_Request, State) -> {noreply, State}.

%%% Default gen_server callbacks. Unused.
handle_info(_Info, State) -> {noreply, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @hidden
%% @doc Emit a new event into the provided buffer.
emit( Event, #state{emp=EMP}=State ) ->
  libemp_monitor_api:emit( Event, EMP ),
  {noreply, State}.

