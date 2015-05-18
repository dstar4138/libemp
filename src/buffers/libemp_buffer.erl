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
-export([start/1, start_link/2]).

%% Export external LibEMP Buffer API
-export([create/1,register/2,unregister/1,
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
             unregister     :: fun(() -> ok),
             ref            :: libemp_buffer_init()
        }).
-record(libemp_buffer_initializer, {
            module          :: atom(),
            init_ref        :: any()
        }).
-type libemp_buffer() :: #libemp_buffer{}.
-type libemp_buffer_init() :: #libemp_buffer_initializer{}.
-export_type([libemp_buffer/0,libemp_buffer_init/0]).

%%% =========================================================================
%%% LibEMP Buffer Behaviour Callbacks
%%% =========================================================================

%% Start up the buffer process tree. The result of this function is passed in
%% on buffer registration. If the buffer implementation does not require a 
%% subprocess tree (and thus nothing to supervise), this function can just 
%% return the arguments for successful registration.
-callback initialize( Args :: [ term() ] ) -> {ok, Ref :: term()} |
                                              {ok, pid(), Ref :: term()} |
                                              {stop, Reason :: term()}.

%% Register with the buffer with a particular direction. The resulting object
%% provides the callbacks the requester requires. The behaviour implementer must
%% construct the buffer object via the libemp_buffer:create/1 function. 
-callback register( Method :: take | give, Ref :: term() ) -> 
                                      {ok, libemp_buffer()} |
                                      {error, Reason :: term()}.

%% Breakdown the buffer process tree safely. This is synonomous with `gen_*' 
%% terminare callback. 
-callback destroy( Ref ::term() ) -> ok.

%%% ------------------------------
%%% Behaviour Object Initialization
%%% ------------------------------

%% @doc Abstract the generation of a LibEMP Buffer Object.
-spec create( [Callback] ) -> {ok, libemp_buffer()} | {error, Reason}
             when Callback :: {FunName, fun()},
                  Reason   :: {missing, FunName} | term(),
                  FunName  :: take | give | size | unregister.
create( Callbacks ) ->
    case {
        get_callback( take, 0, Callbacks ),
        get_callback( give, 1, Callbacks ),
        get_callback( size, 0, Callbacks ),
        get_callback( unregister, 0, Callbacks )
    } of
        {false,_,_,_} -> {error, {missing, take}};
        {_,false,_,_} -> {error, {missing, give}};
        {_,_,false,_} -> {error, {missing, size}};
        {_,_,_,false} -> {error, {missing, unregister}};
        {T,G,S,U} -> 
            {ok, #libemp_buffer{take=T,give=G,size=S,unregister=U}}
    end.

%% @private
%% @doc Initialize the buffer implementation by calling into the callback
%%   module and save the result using the system configuration.
%% @end
-spec start_link( BufferArgs :: [term()], SystemArgs :: [term()] ) ->
                                    {ok, pid(), libemp_buffer_init()} | 
                                    ignore | 
                                    {error, Reason :: term()}.
start_link( BufferInstanceArgs, SystemArgs ) ->
    Args = libemp_util:merge_default_args( BufferInstanceArgs, SystemArgs ),
    case libemp_buffer:start( Args ) of
        {ok, Initializer} ->
            save_buffer( Initializer, SystemArgs ),
            ignore;
        {ok, Pid, Initializer} = Res ->
            save_buffer( Initializer, SystemArgs ),
            verify_link_pid( Pid ),
            Res;
        {error, _} = Err -> 
            Err
    end.
   
%% @private
%% @doc Initialize the buffer; returns a factory initializer for this buffer
%%   type.
%% @end 
-spec start( Args :: [term()] ) -> 
                            {ok, pid(), libemp_buffer_init()} |
                            {ok, libemp_buffer_init()} | 
                            {error, term()}.
start( BufferInstanceArgs ) ->
    {buffer_module, Mod} = proplists:lookup(buffer_module, BufferInstanceArgs),
    case
        catch wrap_extern( Mod, initialize, [BufferInstanceArgs] ) 
    of
        {ok, Ref} ->
            {ok, #libemp_buffer_initializer{module=Mod, init_ref=Ref}};
        {ok, Pid, Ref} ->
            {ok, Pid, #libemp_buffer_initializer{module=Mod, init_ref=Ref}};
        {stop, Reason} -> 
            {error, Reason};
        {'EXIT', Reason} ->
            {error, Reason}
    end.

%% @private
%% @doc Ask the buffer initializer to generate a new buffer instance. 
-spec register( take | give, libemp_buffer_init() ) ->
            {ok, libemp_buffer()} | {error, Reason :: term()}.
register( AsTakeOrGive, 
          #libemp_buffer_initializer{ module=Mod, init_ref=Ref}=Init ) -> 
    case 
        catch wrap_extern( Mod, register, [AsTakeOrGive, Ref] )
    of
        {ok, Buffer} -> 
            {ok, Buffer#libemp_buffer{ref=Init}};
        {'EXIT', Reason} ->
            {error, Reason};
        {error, _} = Err -> 
            Err
    end.

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

%% @doc Unregister the current buffer object. Note, the buffer object will no
%%   longer be valid its usage after has undefined behaviour.
%% @end
-spec unregister( libemp_buffer() ) -> ok.
unregister( #libemp_buffer{unregister=Unregister} ) -> 
    wrap_extern(Unregister,[]).

%% @doc Destroy the buffer. Note, the buffer object will no longer be valid for 
%%   all processes and usage from any object after destruction has undefined
%%   behaviour.
%% @end 
-spec destroy( libemp_buffer() | libemp_buffer_init() ) -> ok.
destroy( #libemp_buffer{ref=BufferInit} ) -> 
    destroy( BufferInit );
destroy( #libemp_buffer_initializer{ module=Mod, init_ref=Ref }=Init ) -> 
    wrap_extern(Mod, destroy, [Ref]), % ignore errors.
    remove_buffer( Init ).


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
verify_link_pid( Pid ) ->
    case catch link( Pid ) of
        true -> {ok, Pid};
        {'EXIT',{noproc,_}} -> {error, noproc}
    end.

