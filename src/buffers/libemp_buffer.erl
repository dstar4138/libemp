%%% LibEMP Buffer Behaviour and Object Wrapper -
%%%
%%%     This module defines two things; the Buffer Behaviour and the Buffer
%%%     Object Wrapper. The Behaviour defined expects only one function which
%%%     initializes the buffer (be that in memory, or starting up a process),
%%%     and then returns it. To do so it must build a transferable structure
%%%     (a record). All interactions with the buffer go through callbacks 
%%%     embedded into this structure. This reduces the complexity of buffer
%%%     implementation and useage, at the cost of portability. 
%%%
%%%     We made this trade off to remove forced implementation bottlenecks
%%%     such as requiring a gen_server as the synchronization point. These
%%%     decisions are better left to the specific buffer developers.
%%%
-module(libemp_buffer).
-compile(inline).
-include("libemp.hrl").

%% Export internal LibEMP Buffer API
-export([start/1,start_link/2]).

%% Export external LibEMP Buffer API
-export([create/1,
         take/1,give/2,size/1,destroy/1,
         flush/1
        ]).

%% LibEMP Buffer Object:
%%  This record defines the passed around buffer object which can be compared
%%  and accessed via this module. Any updates to the API would require a 
%%  change to this record structure. It can be built using the 
%%  libemp_buffer:create/1 function from inside the buffer implementation 
%%  module.
-record(libemp_buffer, { 
             take           :: fun(() -> [libemp_event()]),
             give           :: fun((libemp_event()) -> ok),
             size           :: fun(() -> non_neg_integer()),
             destroy        :: fun(() -> ok)
        }). 
-type libemp_buffer() :: #libemp_buffer{}.
-export_type([libemp_buffer/0]).

%%% =========================================================================
%%% LibEMP Buffer Behaviour Callbacks
%%% =========================================================================

%% Start up the buffer process tree. The result of this function is passed in
%% on buffer registrtion. If the buffer implementation does not require a 
%% subprocess tree, this function can just return the arguments for successful
%% registration.
-callback initialize( Args :: [ term() ] ) -> {ok, Ref :: term()} |
                                              {error, Reason :: term()}.

%% Register with a particular end of the buffer. The resulting buffer object
%% provides the callbacks the requester requires. The behaviour implementer must
%% construct the buffer object via the libemp_buffer:create/1 function. 
-callback register( Method :: take | give, Ref :: term() ) -> 
                                      {ok, libemp_buffer()} |
                                      {stop, Reason :: term()}.

%%% ------------------------------
%%% Behaviour Object Initialization
%%% ------------------------------

%% @doc Abstract the generation of a LibEMP Buffer Object.
-spec create( [Callback] ) -> {ok, libemp_buffer()} | {error, Reason}
             when Callback :: {FunName, fun()},
                  Reason   :: {missing, FunName} | term(),
                  FunName  :: take | give | size | destroy.
create( Callbacks ) ->
    case {
        get_callback( take, 0, Callbacks ),
        get_callback( give, 1, Callbacks ),
        get_callback( size, 0, Callbacks ),
        get_callback( destroy, 0, Callbacks )
    } of
        {false,_,_,_} -> {error, {missing, take}};
        {_,false,_,_} -> {error, {missing, give}};
        {_,_,false,_} -> {error, {missing, size}};
        {_,_,_,false} -> {error, {missing, destory}};
        {T,G,S,D} -> 
            {ok, #libemp_buffer{take=T,give=G,size=S,destroy=D}}
    end.

%% @private
%% @doc Initialize the buffer implementation by calling into the callback
%%   module and save the result using the system configuration.
%% @end
-spec start_link( BufferArgs :: [term()], SystemArgs :: [term()] ) ->
                            {ok, pid()} | ignore | {error, Reason :: term()}.
start_link( BufferInstanceArgs, SystemArgs ) ->
    Args = libemp_util:merge_default_args( BufferInstanceArgs, SystemArgs ),
    {buffer_module, Module} = proplists:lookup(buffer_module, Args),
    case
        catch wrap_extern( Module, initialize, [Args] ) 
    of
        {ok, BufferObj} when is_record(BufferObj, ?MODULE) ->
            save_buffer( BufferObj, SystemArgs ),
            ignore;
        {ok, Pid, BufferObj} when is_record(BufferObj, ?MODULE) andalso
                                  is_pid(Pid) ->
            save_buffer( BufferObj, SystemArgs ),
            link_pid(Pid);
        {stop, Reason} -> 
            {error, Reason}
    end.

start( BufferInstanceArgs ) ->
    {buffer_module, Mod} = proplists:lookup(buffer_module, BufferInstanceArgs),
    case
        catch wrap_extern( Mod, initialize, [BufferInstanceArgs] ) 
    of
        {ok, BufferObj} when is_record(BufferObj, ?MODULE) ->
            {ok, BufferObj};
        {ok, _, BufferObj} when is_record(BufferObj, ?MODULE) ->
            {ok, BufferObj};
        {stop, Reason} -> 
            {error, Reason}
    end.

register( AsTakeOrGive, BufferInitializer ) ->


%%% ------------------------------
%%% Behaviour Object Call-throughs
%%% ------------------------------

%% @doc Optimistically take one or more events off the buffer for consumption.
-spec take( libemp_buffer() ) -> [ libemp_event() ].
take( #libemp_buffer{take=Take} ) -> wrap_extern(Take,[]).

%% @doc Determine the size of the buffer. Only should be used in validation.
-spec size( libemp_buffer() ) -> non_neg_integer().
size( #libemp_buffer{size=Size} ) -> wrap_extern(Size,[]).

%% @doc Push an event into the buffer.
-spec give( libemp_event(), libemp_buffer() ) -> ok.
give( Event, #libemp_buffer{give=Give} ) -> wrap_extern(Give,[Event]).

%% @doc Destroy the buffer. Note, the buffer object will no longer be valid.
-spec destroy( libemp_buffer() ) -> ok.
desbuffer implementation to load.
*troy( #libemp_buffer{destroy=Destroy} = Buffer ) -> 
    wrap_extern(Destroy,[]),
    remove_buffer(Buffer).


%% @doc Drop all events in a buffer until it's size is 0. Only should be used 
%%   in validation, and is not safe for production use (hangs until complete,
%%   even if producers keep pushing events to it).
%% @end
%% TODO: Make into an optional callback?
-spec flush( libemp_buffer() ) -> ok.
flush( Buffer ) ->
    case libemp_buffer:size( Buffer ) of
        0 -> ok;
        _ -> libemp_buffer:take( Buffer ), 
             libemp_buffer:flush( Buffer )
    end.

%%% =========================================================================
%%% Internal Functionality
%%% =========================================================================

%% @hidden
%% @doc Check a proplist for a value, verify it is a function and that it's
%%   arity is correct. Return it if correct, return `false' otherwise.
%% @end
get_callback( FunName, Arity, Callbacks ) ->
    case proplists:lookup(FunName,Callbacks) of
        {_, PossibleFun} when is_function(PossibleFun, Arity) -> PossibleFun;
        _ -> false
    end.

%% @hidden
%% @doc Wrap calls into behaviour implementations. This is were we can 
%%   optionally turn on logging, verbosity, etc.
%% @end
wrap_extern( AnonFun, Args ) -> erlang:apply( AnonFun, Args ).
wrap_extern( Module, FunName, Args ) -> erlang:apply( Module, FunName, Args ). 

%% @hidden
%% @doc Save the buffer according to the LibEMP System configuration. This may
%%   involve dumping the reference into an ets table.
%% @end
save_buffer( _BufferObj, _SystemArgs ) -> ok. %TODO: Save the libemp buffer obj.

%% @hidden
%% @doc Remove the buffer according to the LibEMP System Configuration.
remove_buffer( _BufferObj ) -> ok. %TODO: Destroy the saved libemp buffer obj. 

%% @hidden
%% @doc Link a process identifier, but if there is an error doing so return it. 
link_pid( Pid ) ->
    case catch link( Pid ) of
        true -> {ok, Pid};
        {'EXIT',{noproc,_}} -> {error, noproc}
    end.

