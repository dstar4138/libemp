%%% LibEMP Node State Server -
%%%
%%%    Monitor the local ETS tables for this LibEMP Node's state. Remember
%%%    that the LibEMP model for clustering is to use a distributed buffer
%%%    rather than relying on the VM implementation of Distributed Erlang. 
%%%
-module(libemp_node).
-behaviour(gen_server).

-include("internal.hrl").

%% API
-export([start_link/0]).
-export([save_buffer/2,save_buffer/3]).
-export([remove_buffer/1]).
-export([get_buffer/0,get_buffer/1]).

-export([save_monitor/2]).
-export([remove_monitor/1]).
-export([get_monitor/1]).

-export([
  get_default_proc/1,
  save_default_proc/2,
  remove_default_proc/2
]).

-export([
  inject/2,
  get_application/1,
  get_applications/0,
  stop_all_applications/0,
  stop_application/1
]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Table Default Options
-define(DEFAULT_TABLE_OPS,[
        ordered_set, % We will sort by ID (integer or named atoms).
        protected,   % Synchronize writes through this process.
        {read_concurrency, true}, % Favor reads (as this will be most common).
        named_table  % Provided so that other processes may access without Ref.
]).

%% Local Node Server State:
%%  Stores a reference to all of the local state tables in ETS.
-record(state, {apptab, buftab, montab, proctab}).

%%% ------------------
%%% Node State Tables
%%% ------------------

%% Node Default Processor References:
%%  When a processor is created, we optionally save a reference to it if it is
%%  the first one started for a given buffer. You interact with this table via
%%  the libemp_processor module rather than via this module directly. See the
%%  following:
%%      libemp_processor:start_link/3
%%      libemp_processor:stop/2
-define(LIBEMP_NODE_PROCESSORS, libemp_node_processors).
-record(libemp_node_processors, {id, pid, buffer_id}).

%% @doc Save the processor to be the default for a given buffer reference, as
%%    long as there wasn't one already saved.
%% @end
-spec save_default_proc( term(), pid() ) -> ok | {error, any()}.
save_default_proc( Buffer, Pid ) ->
  case get_default_proc( Buffer ) of
    none -> % Do insert
      Row = #libemp_node_processors {
            id = erlang:make_ref(),
            pid = Pid,
            buffer_id = libemp_buffer:get_id( Buffer )
      },
      gen_server:call( ?MODULE, {save_proc, Row} );
    _ -> ok
  end.

%% @doc Get the default processor for the given buffer.
-spec get_default_proc( term() ) -> none | pid().
get_default_proc( Buffer ) ->
  ID = libemp_buffer:get_id( Buffer ),
  Processor = ets:select(?LIBEMP_NODE_PROCESSORS,
                         [{#libemp_node_processors{pid='$1', buffer_id=ID, _='_'},
                              [], ['$1']}]),
  case Processor of
    [] -> none;
    [Pid] -> Pid
  end.

%% @doc Remove the default processor for the given Buffer.
-spec remove_default_proc( term(), pid() ) -> ok | {error, any()}.
remove_default_proc( Buffer, Pid ) ->
  BufferId = libemp_buffer:get_id( Buffer ),
  gen_server:call(?MODULE, {delete_proc, BufferId, Pid}).

%% Node Buffer Initializers:
%%  When a buffer is created, we save a registration mechanism in the table
%%  which can be used by Sinks and Monitors to register with the active buffer.
%%  You interact with this table via the libemp_buffer module rather than via
%%  this module directly. See the following:
%%      libemp_buffer:start/1
%%      libemp_buffer:start_link/1
%%      libemp_buffer:get_buffer/1,2
%%      libemp_buffer:destroy/1
%%      libemp_buffer:(de)activate/1
-define(LIBEMP_NODE_BUFFERS, libemp_node_buffers).
-record(libemp_node_buffers, {id, initializer, active = true}).

%% @doc Save an initializer for a particular running buffer instance. If the
%%   buffer should have a custom ID or start deactivated, that can be 
%%   customized. The end result of this call is that Monitors and Sinks will
%%   be able to be wired up to the running instance (if active). 
%% @end
-spec save_buffer( Active :: boolean(),
                   ID, 
                   libemp_buffer:libemp_buffer_init() ) -> {ok,ID} | error
                            when ID :: term().
save_buffer( ID, Initializer ) -> 
    save_buffer( true, ID, Initializer ).
save_buffer( Active, ID, Initializer ) ->
    Row = #libemp_node_buffers{ id=ID, 
                                initializer=Initializer, 
                                active=Active },
    case gen_server:call(?MODULE, {save_buffer, Row} ) of
         ok -> {ok, ID};
         error -> error
    end.

%% @doc Delete a buffer from the table (i.e. on buffer shutdown). Monitors
%%   and Sinks may still think they are wired to it.
%% @end
-spec remove_buffer( ID :: term() ) -> ok | error. 
remove_buffer( ID ) ->
    gen_server:call(?MODULE, {delete_buffer, ID}).

%% @doc Returns the top buffer unless an ID is provided. If no buffer is
%%   provided it will return an error.
%% @end
-spec get_buffer( ID :: term() ) -> {ok, libemp_buffer:libemp_buffer_init()} |
                                    {error, {badarg, term()}}.
get_buffer() -> get_buffer( ets:first( ?LIBEMP_NODE_BUFFERS ) ).
get_buffer( ID ) -> 
    case ets:lookup( ?LIBEMP_NODE_BUFFERS, ID ) of
       []    -> {error, {badarg, ID}};
       [Row] -> {ok, Row#libemp_node_buffers.initializer}
    end. 

%% Note Monitor Platform References:
%%  When a Monitor is created a link is made to an existing buffer, as well as
%%  a service which contains the Monitor state. This is bundled into a Platform
%%  Reference we can use to reference an actual running instance of a Monitor.
%%  You interact with this table via the libemp_monitor module rather than via
%%  this module directly. See the following:
%%      libemp_monitor:start/3
%%      libemp_monitor:start_link/3
%%      libemp_monitor:stop/1
%%      libemp_monitor:get_monitor/1
-define(LIBEMP_NODE_MONITORS, libemp_node_monitors).
-record(libemp_node_monitors, {name, pr}).

%% @doc Save the Monitor configuration and platform ref for de-initialization.
save_monitor( Name, PlatformRef ) ->
  Row = #libemp_node_monitors{ name = Name, pr = PlatformRef },
  gen_server:call(?MODULE, {save_monitor, Row}).

%% @doc Remove the references Monitor from the Node's database.
remove_monitor( #monitorref{name = Name} ) ->
  gen_server:call(?MODULE, {delete_monitor, Name});
remove_monitor( Name ) ->
  gen_server:call(?MODULE, {delete_monitor, Name}).

%% @doc Get the MonitorRefrence and the process identifier of the named Monitor.
get_monitor( #monitorref{pid = Pid}=PR ) ->
  {ok,Pid,PR}; %% Overload hack to hide complexities elsewhere.
get_monitor( Name ) ->
  case ets:lookup( ?LIBEMP_NODE_MONITORS, Name )  of
    [] -> {error, {badarg, Name}};
    [Row] ->
      PR = Row#libemp_node_monitors.pr,
      Pid = PR#monitorref.pid,
      {ok, Pid, PR}
  end.

%% Note Application References:
%%  When an application is created and installed. You interact with this table
%%  bia the libemp module rather than via this module directly. See the
%%  following:
%%      libemp:start/2
%%      libemp:stop/0,1
%%      libemp:which_applications/0
-define(LIBEMP_NODE_APPLICATIONS, libemp_node_applications).
-record(libemp_node_applications, {name, app_def, app_refs}).

%% @doc Get a list of the Applications that have been loaded onto the local
%%    LibEMP node.
%% @end
get_applications() ->
  ets:select( ?LIBEMP_NODE_APPLICATIONS,
              [ {#libemp_node_applications{name='$1', _='_'}, [], ['$1']} ] ).

%% @doc Get the Application Definition for a specified Application loaded onto
%%    the local LibEMP node.
%% @end
get_application( AppName ) ->
  case ets:lookup( ?LIBEMP_NODE_APPLICATIONS, AppName ) of
    [] -> {error,{badarg,AppName}};
    [Row] ->
      AppDef = Row#libemp_node_applications.app_def,
      {ok, AppDef}
  end.

%% @doc Safely stop all of the applications on the local node while leaving the
%%   Node alive. This should put the local node back to a clean state.
%% @end
stop_all_applications() ->
  lists:foldl(fun(App,_) -> stop_application(App) end, ok, get_applications()).

%% @doc Stop the LibEMP Application specified, this leaves all other
%%    applications running.
%% @end
stop_application( AppName ) ->
  % Unfold the application:
  [Refs] = ets:lookup( ?LIBEMP_NODE_APPLICATIONS, AppName ),
  AppRefs = Refs#libemp_node_applications.app_refs,
  libemp_app_exec:uninstall( normal, AppRefs ),
  gen_server:call( ?MODULE, {delete_application, AppName} ).

%% @doc Inject the Application onto the local libemp_node.
inject( AppName, AppDef ) ->
  case libemp_app_exec:install( AppDef ) of
    {ok, AppRefs} ->
      App = #libemp_node_applications {
        name=AppName, app_def=AppDef, app_refs=AppRefs
      },
      gen_server:call( ?MODULE, {save_application, App} );
    Error -> Error
  end.

%%% ===================================================================
%%% API
%%% ===================================================================

%% @doc Starts the server
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server (and all the private tables for this node).
init([]) ->
    case build_tables() of
        {error, _}=E -> E;
        State -> {ok, State}
    end. 

%% @private
%% @doc Handling call messages
handle_call({save_buffer,#libemp_node_buffers{}=R},_From,#state{buftab=BT}=S)->
    insert(BT,R,S);
handle_call({remove_buffer,ID},_From,#state{buftab=BT}=S)->
    delete(BT,ID,S);
handle_call({save_monitor,#libemp_node_monitors{}=R},_From,#state{montab=MT}=S) ->
    insert(MT,R,S);
handle_call({remove_monitor,Name},_From,#state{montab=MT}=S)->
    delete(MT,Name,S);
handle_call({save_application,#libemp_node_applications{}=R},_From,#state{apptab=AT}=S) ->
    insert(AT,R,S);
handle_call({delete_application,AppName},_From,#state{apptab=AT}=S) ->
    delete(AT,AppName,S);
handle_call({save_proc,#libemp_node_processors{}=R},_From,#state{proctab=PT}=S) ->
    insert(PT,R,S);
handle_call({delete_proc,BID,_Pid},_From,#state{proctab=PT}=S) ->
    delete(PT,BID,S);
handle_call(Request, From, State) ->
    ?ERROR("Bad Request to Node Server from (~p): ~p~n",[From,Request]),
    {reply, {error, badreq}, State}.

%% @private
%% @doc Handling cast messages
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
%% @doc Cleans up the state server before bringing everything down.
terminate(_Reason, State) ->
    destroy_tables( State ).

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

insert( Table, Row, State ) ->
  true = ets:insert(Table, Row),
  {reply, ok, State}.
delete( Table, ID, State ) ->
  true = ets:delete( Table, ID ),
  {reply, ok, State}.

%% @hidden
%% @doc Create the tables that we will be using by the rest of LibEMP.
build_tables() ->
    case catch 
        {
            build_applications_table(),
            build_buffers_table(),
            build_monitors_table(),
            build_processor_table()
        } 
    of
        {'EXIT',Reason} -> 
            {error,Reason};
        
        % Assume Successful handling.
        {
         AppsTable,
         BufTable,
         MonitorTable,
         ProcessorTable
        } -> 
            #state{ 
                apptab=AppsTable,
                buftab=BufTable,
                montab=MonitorTable,
                proctab=ProcessorTable
            }
    end.

%% @hidden
%% @doc Create the Applications table.
build_applications_table() ->
  Options = [ {keypos, #libemp_node_applications.name} | ?DEFAULT_TABLE_OPS ],
  ets:new( ?LIBEMP_NODE_APPLICATIONS, Options ).

%% @hidden
%% @doc Create the buffer table.
build_buffers_table() ->
    Options = [ {keypos, #libemp_node_buffers.id} | ?DEFAULT_TABLE_OPS ],
    ets:new( ?LIBEMP_NODE_BUFFERS, Options ).

%% @hidden
%% @doc Create the Monitor reference table.
build_monitors_table() ->
    Options = [ {keypos, #libemp_node_monitors.name} | ?DEFAULT_TABLE_OPS ],
    ets:new( ?LIBEMP_NODE_MONITORS, Options ).

%% @hidden
%% @doc Create the Processor reference table.
build_processor_table() ->
  Options = [ {keypos, #libemp_node_processors.buffer_id} | ?DEFAULT_TABLE_OPS ],
  ets:new( ?LIBEMP_NODE_PROCESSORS, Options ).

%% @hidden
%% @doc Delete all state tables on shutdown, this will not clean up the 
%%   Buffer or wiring state of the system, just the registration system.
%% @end
destroy_tables( #state{ apptab=AT, buftab=BT, montab=MT, proctab=PT } ) ->
    true = ets:delete(AT),
    true = ets:delete(BT),
    true = ets:delete(MT),
    true = ets:delete(PT).


