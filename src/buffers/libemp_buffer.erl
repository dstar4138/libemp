%%% LibEMP Buffer Behaviour and Object Wrapper -
%%%
%%%     This module defines two things; the Buffer Behaviour and the Buffer
%%%     Object Wrapper. The Behaviour defined expects only one function which
%%%     initializes the buffer (be that in memory, or starting up a process),
%%%     and then returns it. To do so, it must build a transferable structure
%%%     (a record). All interactions with the buffer go through callbacks 
%%%     embedded into this structure. This reduces the complexity of buffer
%%%     implementation and useage, at the cost of portability. 
%%%
%%%     We made this trade off to remove forced implementation bottlenecks
%%%     such as requiring a gen_server as the synchronization point. These
%%%     decisions are better left to the specific buffer developers.
%%%
%%%     Usage:
%%%         > Example = libemp_simple_buffer.
%%%         > {ok, Ref, Buffer} = libemp_buffer:start( Example ).
%%%         > {ok, ProducerRef} = libemp_buffer:register( give, Buffer ).
%%%         > {ok, ConsumerRef} = libemp_buffer:register( take, Buffer ).
%%%         > ok = libemp_buffer:give( Event, ProducerRef ).
%%%         > [ Event ] = libemp_buffer:take( ConsumerRef ).
%%%         ...
%%%         > ok = libemp_buffer:destroy( ProducerRef ).
%%%         > ok = libemp_buffer:destroy( ConsumerRef ).
%%%         > ok = libemp_buffer:destroy( Buffer ).
%%%
-module(libemp_buffer).
-compile(inline).
-include("../internal.hrl").

%% Export internal LibEMP Buffer API
-export([start/1, start/2, start/3,
         start_link/3, validate_configs/3]).

%% Export external LibEMP Buffer API
-export([create/1,register/2,unregister/1,
         take/1,give/2,size/1,destroy/1,
         flush/1,drop/2
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
            id              :: term(), % Globally unique ID for referencing.
            module          :: atom(), % Module which defines behaviour.
            init_ref        :: any()   % Reference state for the initializer. 
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
%% terminate callback. 
-callback destroy( Ref ::term() ) -> ok.

%% OPTIONAL:
-optional_callbacks([ validate_configs/1 ]).

%% Check if the arguments getting passed in are going to be valid. You can
%% be as strict as you want here. But know LibEMP startup will fail if this
%% returns an Error.
-callback validate_configs( [ term() ] ) -> ok | {error, Reason :: term()}.

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
%% @doc Checks if the given parameters match up with a particular buffer
%%   which can be loaded and executed. If the buffer module exports config
%%   validation api, then it will check it via that as well.
%% @end
-spec validate_configs( atom(), atom(), [term()] ) -> ok | {error, term()}.
validate_configs( Name, Module, Configs ) ->
  case {
    libemp_node:get_buffer(Name),
    code:which(Module)
  }
  of
    { {ok,_}, _ } ->
      {error, {buffer_already_exists,Name}};
    { _, non_existing } ->
      {error, {non_existing, Module}};
    _Success ->
      libemp_util:wrap_extern( Module, validate_configs, [ Configs ], ok )
  end.


%% @private
%% @doc Initialize the buffer implementation by calling into the callback
%%   module and save the result using the system configuration. This allows
%%   Sinks/Monitors to link to it by name rather than by reference (which is
%%   required if started via start/1,2,3).
%% @end
-spec start_link( Name :: atom(), Mod :: atom(), BufferArgs :: [term()] ) ->
                                    {ok, pid()} |
                                    ignore | 
                                    {error, Reason :: term()}.
start_link( Name, Mod, Args ) ->
    case libemp_buffer:start( Name, Mod, Args ) of
        {ok, false, Initializer} ->
            save_buffer( Initializer ),
            ignore;
        {ok, Pid, Initializer} ->
            save_buffer( Initializer ),
            verify_link_pid( Pid ),
            {ok, Pid};
        {error, _} = Err -> 
            Err
    end.


%% @private
%% @doc Initialize the buffer with default arguments; returns a factory
%%   initializer for this buffer type.
%% @end
-spec start( Module ::atom() ) ->
                            {ok, pid(), libemp_buffer_init()} |
                            {ok, false, libemp_buffer_init()} |
                            {error, term()}.
start( Module ) -> start( Module, Module, [] ).

%% @private
%% @doc Initialize the buffer with alternative arguments;
%%   returns a factory initializer for this buffer type.
%% @end
-spec start( Name :: atom(), Module ::atom() ) ->
                            {ok, pid(), libemp_buffer_init()} |
                            {ok, false, libemp_buffer_init()} |
                            {error, term()}.
start( Module, Args ) -> start( Module, Module, Args ).

%% @private
%% @doc Initialize the buffer; returns a factory initializer for this buffer
%%   type. Useful if you have multiple buffers with different arguments and
%%   need them all running at the same time.
%% @end 
-spec start( Name :: atom(), Module ::atom(), Args :: [term()] ) ->
                            {ok, pid(), libemp_buffer_init()} |
                            {ok, false, libemp_buffer_init()} |
                            {error, term()}.
start( Name, Mod, BufferInstanceArgs ) ->
    case
        catch libemp_util:wrap_extern( Mod, initialize, [BufferInstanceArgs] )
    of
        {ok, Ref} ->
            {ok, false, new_initializer(Name, Mod, Ref)};
        {ok, Pid, Ref} ->
            {ok, Pid, new_initializer(Name, Mod, Ref)};
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
        catch libemp_util:wrap_extern( Mod, register, [AsTakeOrGive, Ref] )
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
take( #libemp_buffer{take=Take} ) -> libemp_util:wrap_extern(Take,[]).

%% @doc Determine the size of the buffer. Only should be used in validation.
-spec size( libemp_buffer() ) -> non_neg_integer().
size( #libemp_buffer{size=Size} ) -> libemp_util:wrap_extern(Size,[]).

%% @doc Push an event into the buffer.
-spec give( libemp_event(), libemp_buffer() ) -> ok.
give( Event, #libemp_buffer{give=Give} ) ->
    libemp_util:wrap_extern(Give,[Event]).

%% @doc Unregister the current buffer object. Note, the buffer object will no
%%   longer be valid its usage after has undefined behaviour.
%% @end
-spec unregister( libemp_buffer() ) -> ok.
unregister( #libemp_buffer{unregister=Unregister} ) -> 
    libemp_util:wrap_extern(Unregister,[]).

%% @doc Destroy the buffer. Note, the buffer object will no longer be valid for 
%%   all processes and usage from any object after destruction has undefined
%%   behaviour.
%% @end 
-spec destroy( libemp_buffer() | libemp_buffer_init() ) -> ok.
destroy( #libemp_buffer{ref=BufferInit} ) -> 
    destroy( BufferInit );
destroy( #libemp_buffer_initializer{ module=Mod, init_ref=Ref }=Init ) -> 
    libemp_util:wrap_extern(Mod, destroy, [Ref]), % ignore errors.
    remove_buffer( Init ).

%% @doc Drop all events in a buffer until it's size is 0. Only should be used
%%   in validation, and is not safe for production use (hangs until complete,
%%   even if producers keep pushing events to it).
%% @end
-spec flush( libemp_buffer() ) -> ok.
flush( Buffer ) -> libemp_buffer:drop( Buffer, -1 ).

%% @doc Performs N take/1 operations from the queue, effectively drops N events,
%%   unless take is in 'batch' mode. In that case, it would take N batches. This
%%   is a safer "flush" which will let you decide how long you are willing to
%%   wait to flush.
%% @end
-spec drop( libemp_buffer(), integer() ) -> ok.
drop( _, 0 ) -> ok;
drop( Buffer, Itters ) ->
    case libemp_buffer:size( Buffer ) of
        0 -> ok;
        _ -> libemp_buffer:take( Buffer ), 
             libemp_buffer:drop( Buffer, Itters - 1 )
    end.

%%% =========================================================================
%%% Internal Functionality
%%% =========================================================================

%% @hidden
%% @doc Create a new initializer reference object for a buffer.
new_initializer( Name, Module, Ref ) ->
  #libemp_buffer_initializer{id=Name, module=Module, init_ref=Ref}.

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
%% @doc Save the buffer according to the LibEMP System configuration. This
%%   involves pulling out the unique ID and saving it to the Node's State
%%   Server. 
%% @end
save_buffer( #libemp_buffer_initializer{id=undefined}=BufferObj ) ->
    NewUniqueID = erlang:unique_integer([positive]),
    libemp_node:save_buffer(
      NewUniqueID, 
      BufferObj#libemp_buffer_initializer{id=NewUniqueID} 
    );
save_buffer( #libemp_buffer_initializer{id=ID}=BufferObj ) ->
    % We already have an ID, so push.
    libemp_node:save_buffer( ID, BufferObj ).

%% @hidden
%% @doc Remove the buffer according to the LibEMP System Configuration.
remove_buffer( #libemp_buffer_initializer{id=ID} ) -> 
    catch libemp_node:remove_buffer( ID ), % Hide errors, may not be saved.
    ok.

%% @hidden
%% @doc Link a process identifier, but if there is an error doing so return it. 
verify_link_pid( Pid ) ->
    case catch link( Pid ) of
        true -> {ok, Pid};
        {'EXIT',{noproc,_}} -> {error, noproc}
    end.

