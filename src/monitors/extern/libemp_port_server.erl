%%% LibEMP Port Server -
%%%
%%%   A generic server to monitor a port exit signals and events. For
%%%   every stdout line we encounter we'll emit an event into LibEMP.
%%%
-module(libemp_port_server).
-behaviour(gen_server).

%% API
-export([
  start_link/4,
  send_command/2,
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

-define(EVENT(Name, Data), {port_event, Name, Data}).
-record(state, {name, emp, port}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the port server, with the given name.
start_link(Name, EMP, PortName, PortConfigs) ->
  gen_server:start_link( ?MODULE, [
    Name, EMP, PortName, PortConfigs
  ], [] ).

%% @doc Send a command over stdin to port.
send_command( Command, Pid ) ->
  gen_server:cast( Pid, {command, Command} ).

%% @doc Stop the port server.
stop( Reason, Pid ) ->
  gen_server:cast( Pid, {shutdown, Reason} ).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
init([Name, EMP, PortName, PortConfigs]) ->
  process_flag(trap_exit, true),
  build_state(Name, EMP, PortName, PortConfigs).

%% @private
%% @doc Handling call/cast messages.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast( {shutdown,_}=Reason, State ) -> {stop, Reason, State};
handle_cast( {command, Cmd}, State ) -> send_port( Cmd, State );
handle_cast(_Request, State) -> {noreply, State}.

%% @private
%% @doc Handle port messages.
handle_info({_Port, {data, Data}}, State) -> notify(Data, State);
handle_info({_Port, {exit_status, _}}, State) -> {stop, shutdown, State};
handle_info({'EXIT', _Port, Reason}, State) -> {stop, Reason, State};
handle_info(_Info, State) -> {noreply, State}.

%% @private
%% @doc Handle Server termination by validating Port closure.
terminate(Reason, #state{port=Port}=State) ->
  _ = notify({shutdown,Reason},State),
  catch erlang:port_close( Port ).

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%% @doc Build the port and create the server state.
build_state( Name, EMP, PortName, PortConfigs ) ->
  case erlang:open_port( PortName, PortConfigs ) of
    Port when is_port( Port ) ->
      {ok, state(Name, EMP, Port)};
    Error ->
      {error, Error}
  end.

%% @hidden
%% @doc Create the state object.
state( Name, EMP, Port ) ->
  #state{name=Name, emp=EMP, port=Port}.

%% @hidden
%% @doc Push the event to
notify( Data, #state{name=Name,emp=EMP}=State ) ->
  libemp_monitor_api:emit( ?EVENT(Name, Data), EMP ),
  {noreply, State}.

%% @hidden
%% @doc Send a message to port via stdin.
send_port( Msg, #state{port=Port}=State ) ->
  erlang:port_command( Msg, Port ),
  {noreply, State}.

