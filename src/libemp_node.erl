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

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Local Node Server State:
%%  Stores a refrence to all of the local state tables in ETS.
-record(state, {buftab}).

%%% ------------------
%%% Node State Tables
%%% ------------------

%% Node Buffer Intializers: 
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
    save_buffer( ID, Initializer, true ).
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
    case ets:select( ?LIBEMP_NODE_BUFFERS, ID ) of
       []    -> {error, {badarg, ID}};
       [Buf] -> {ok, Buf}
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
    case ets:insert(BT,R) of
        true ->
           {reply, ok, S};
        _ -> %TODO: Clean up error response
           {reply, error, S}
    end;
handle_call({remove_buffer,ID},_From,#state{buftab=BT}=S)->
    case ets:delete(BT,ID) of
        true ->
           {reply, ok, S};
        _ -> %TODO: Clean up error response
           {reply, error, S}
    end;
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

%% @hidden
%% @doc Create the tables that we will be using by the rest of LibEMP.
build_tables() ->
    case catch 
        { 
            build_buffers_table()
        } 
    of
        {'EXIT',Reason} -> 
            {error,Reason};
        
        % Assume Successful handling.
        { 
         BufTable
        } -> 
            #state{ 
                buftab=BufTable 
            }
    end.

%% @hidden
%% @doc Create the buffer table.
build_buffers_table() ->
    Options = [
        ordered_set, % We will sort by ID (integer or named atoms).
        protected,   % Synchronize writes through this process.
        {read_concurrency, true}, % Favor reads (as this will be most common).
        named_table  % Provided so that other processes may access without Ref.
    ],
    ets:new( ?LIBEMP_NODE_BUFFERS, Options ).

%% @hidden
%% @doc Delete all state tables on shutdown, this will not clean up the 
%%   Buffer or wiring state of the system, just the registration system.
%% @end
destroy_tables( #state{ buftab=BT} ) ->
    true = ets:delete(BT).

