%%% LibEMP Wire Config Parsing/Validation -
%%%
%%%     Parse out configs from wire files for loading up LibEMP applications.
%%%
-module(libemp_wire).

%% Public exports
-export([parse/1, parse/2,
         validate/1]).
-export([get_buffers/1,
         get_sinks/1,
         get_monitors/1,
         get_stacks/1,
         get_fault_funs/1
        ]).

%% Private exports.
-export([new_env/0, new_env/5,
         parse_opt/2, parse_file/2]).

-record(libemp_env, {
            buffers = #{},
            monitors = #{},
            sinks = #{},
            fault_funs = #{},
            stacks = []
         }).
-type libemp_env() :: #libemp_env{}.

% Utility types, TODO: clean up!
-type configs() :: [ {atom(), term()} ].
-type named_sink() :: atom() | {atom(), fault_function()}.
-type fault_function() :: libemp_sink_stack:fault_handler(). 
-type potentially_named_config(Name) :: {Name, atom(), configs()} |
                                        {Name, atom(), atom(), configs()}.

-type buffer_config()  :: potentially_named_config( buffer ). 
-type sink_config()    :: potentially_named_config( sink ). 
-type monitor_config() :: potentially_named_config( monitor ).
-type include_config() :: {include, file:name_all()}.
-type stack_config()   :: {stack, [ named_sink() ]} | 
                          {sink, fault_function(), [ named_sink() ]}.
-type fault_function_config() :: {fault_function, atom(), fault_function()}.
-type config_option() :: buffer_config() | sink_config() | stack_config() |
                         fault_function_config() | monitor_config() | 
                         include_config().

-export_type([libemp_env/0, config_option/0]).

%%% =======================================================================
%%% Public API
%%% =======================================================================

%% @doc Validate/Parse the configs. The config list can be a list of file paths 
%%    (absolute or relative), or a list of n-ary tuples which denote the wire 
%%    config language (i.e. result of file:consult/1 on a *.wire file).
%% @end
-spec parse( Config ) -> {ok, libemp_env()} | {error, Reason}
            when Reason :: term(),
                 Config :: [ file:name_all() | libemp_wire:config_option() ].
parse( Config ) -> 
    parse( Config, new_env() ).
parse( Config, Env ) -> 
    libemp_util:escaping_foldl( fun parse_item/2, Env, Config ).

%% @doc Run a series of tests on the environment to see if everything is in
%%      order.
%% @end
-spec validate( libemp_env() ) -> {ok, libemp_env()} | {error, term()}.
validate( Env ) ->
    libemp_util:escaping_foldl(fun( F, E ) -> F(E) end, Env, [
        fun check_buffer_exists/1,          % Must have a buffer defined
        fun check_monitor_or_sink_exists/1, % Must either consume/produce
        fun check_monitor_to_buffer_link/1, % Monitors use existing buffers
        fun check_stack_to_buffer_link/1,   % Sinks use existing buffers
        fun check_sink_to_sink_links/1      % Stacks use existing sinks
    ]).

%% @doc Abstracted accessor for getting buffers from Environment object.
-spec get_buffers( libemp_env() ) -> map().
get_buffers( #libemp_env{ buffers=B } ) -> B.

%% @doc Abstracted accessor for getting monitors from Environment object.
-spec get_monitors( libemp_env() ) -> map().
get_monitors( #libemp_env{ monitors=M } ) -> M.

%% @doc Abstracted accessor for getting sinks from Environment object.
-spec get_sinks( libemp_env() ) -> map().
get_sinks( #libemp_env{ sinks=S } ) -> S.

%% @doc Abstracted accessor for getting stacks from Environment object.
-spec get_stacks( libemp_env() ) -> list().
get_stacks( #libemp_env{ stacks=S } ) -> S.

%% @doc Abstracted accessor for getting fault funs from Environment object.
-spec get_fault_funs( libemp_env() ) -> map().
get_fault_funs( #libemp_env{ fault_funs=F } ) -> F.

%%% =======================================================================
%%% Private API
%%% =======================================================================

%% @private
%% @doc Construct an empty LibEMP Environment that we can modify with options.
new_env() -> #libemp_env{}.
new_env( Buffers, Monitors, Sinks, Stacks, FaultFuns ) ->
    #libemp_env{ buffers=Buffers, 
                 monitors=Monitors, 
                 sinks=Sinks, 
                 stacks=Stacks, 
                 fault_funs=FaultFuns }.

%% @private
%% @doc Parse either a file or config option given an environment.
parse_item( Opt, Env ) when is_tuple( Opt ) -> parse_opt( Opt, Env );
parse_item( FileName, Env ) -> parse_file( FileName, Env).

%% @private
%% @doc Parse a wire configuration option and update the environment.
parse_opt( {buffer, Module, Configs}, #libemp_env{buffers=B}=Env ) ->
    NewBuffer = {Module, Configs},
    UpdatedBuffers = B#{Module => NewBuffer}, 
    Env#libemp_env{ buffers=UpdatedBuffers };
parse_opt( {buffer, Name, Module, Configs}, #libemp_env{buffers=B}=Env ) -> 
    NewBuffer = {Module, Configs},
    UpdatedBuffers = B#{Name => NewBuffer}, 
    Env#libemp_env{ buffers=UpdatedBuffers };
parse_opt( {monitor, Module, Configs}, #libemp_env{monitors=M}=Env )->
    CurrentBufferLinks = maps:get(M,default,[]),
    NewLinks = [{Module,Configs}|CurrentBufferLinks],
    NewMonitors = M#{default =>NewLinks},
    Env#libemp_env{ monitors=NewMonitors };
parse_opt( {monitor, Buffer, Module, Configs}, #libemp_env{monitors=M}=Env ) -> 
    CurrentBufferLinks = maps:get(M,Buffer,[]),
    NewLinks = [{Module,Configs}|CurrentBufferLinks],
    NewMonitors = M#{Buffer =>NewLinks},
    Env#libemp_env{ monitors=NewMonitors };
parse_opt( {sink, Module, Configs}, #libemp_env{sinks=S}=Env ) ->
    NewSink = {Module,Configs},
    NewSinks = S#{Module => NewSink}, 
    Env#libemp_env{ sinks=NewSinks };
parse_opt( {sink, Name, Module, Configs}, #libemp_env{sinks=S}=Env ) -> 
    NewSink = {Module,Configs},
    NewSinks = S#{Name => NewSink}, 
    Env#libemp_env{ sinks=NewSinks };
parse_opt( {stack, SinkList}, #libemp_env{stacks=S}=Env ) ->
    BufferName = default,
    Handler = default,
    NewStack = {BufferName, Handler, SinkList}, 
    NewStackSet = [NewStack|S],
    Env#libemp_env{ stacks=NewStackSet };
parse_opt( {stack, BufferName, SinkList}, #libemp_env{stacks=S}=Env ) ->
    Handler = default,
    NewStack = {BufferName, Handler, SinkList}, 
    NewStackSet = [NewStack|S],
    Env#libemp_env{ stacks=NewStackSet };
parse_opt( {stack, DefaultHandler, BufferName, SinkList}, #libemp_env{stacks=S}=Env ) -> 
    NewStack = {BufferName, DefaultHandler, SinkList}, 
    NewStackSet = [NewStack|S],
    Env#libemp_env{ stacks=NewStackSet };
parse_opt( {fault_function, HandlerName, Fun},#libemp_env{fault_funs=F}=Env) ->
    NewFuns = F#{HandlerName => Fun},
    Env#libemp_env{ fault_funs=NewFuns };
parse_opt( {include, RelativeOrAbsolutePathToWireFile}, Env) ->
    parse_file( RelativeOrAbsolutePathToWireFile, Env );
parse_opt( Unknown, Env ) ->
    {error, {badarg, Unknown, Env}}.

%% @private
%% @doc Parse a file given a filename and an environment.
parse_file( FileName, Env ) ->
    case file:consult( FileName ) of
        {ok, Configs} -> parse( Configs, Env );
        {error,_}=Err -> Err
    end.

%%% =======================================================================
%%% Internal API
%%% =======================================================================

%% @hidden
%% @doc Check whether the environment has a buffer defined.
check_buffer_exists( #libemp_env{buffers=B}=E ) ->
    case maps:size( B ) > 0 of true -> E; false -> {error,nobuffer} end.

%% @hidden
%% @doc Check whether the node is either a processing or computation node,
%%   there must be at least one.
%% @end
check_monitor_or_sink_exists( #libemp_env{monitors=M,sinks=S}=E ) ->
    case maps:size( M ) + maps:size( S ) > 0 of 
        true -> E; 
        false -> {error, no_monitor_or_sink}
    end.

%% @hidden
%% @doc Check that all the monitors have defined a buffer which exists.
check_monitor_to_buffer_link( #libemp_env{monitors=M,buffers=B}=E ) ->
    MVals = maps:values(M),
    case indexes_exist_as_key( MVals, 1, B, missing_buffer_for_monitor ) of
        true -> E;
        Error -> Error 
    end.

%% @hidden
%% @doc Check that all the Sink Stacks have defined a buffer which exists.
check_stack_to_buffer_link( #libemp_env{stacks=S,buffers=B}=E ) ->
    case indexes_exist_as_key( S, 1, B, missing_buffer_for_stack ) of
       true -> E;
       Error -> Error
    end.

%% @hidden
%% @doc Check that all the Sinks in all Stacks have been defined.
check_sink_to_sink_links( #libemp_env{stacks=Stacks,sinks=Sinks} ) ->
    {_BNames, _Handlers, Sinks} = lists:unzip(Stacks),
    SinkNames = proplists:get_keys( lists:flatten( Sinks ) ),
    case libemp_util:escaping_foldl( fun check_index/2, Sinks, SinkNames ) of
        {error,Reason} -> {error, {missing_sink, Reason}};
        _ -> true
    end.


%% @hidden
%% @doc For each tuple in a list, make sure the n'th element is a key in the
%%   provided map. Otherwise return the error given.
%% @end
indexes_exist_as_key( Map1Values, Index, Map2, ErrType ) ->
    Map1VUnzip = lists:unzip(Map1Values),     % [As, Bs, Cs]
    Map1VIndex = lists:nth(Index, Map1VUnzip),% if I=2, Bs, etc.
    case libemp_util:escaping_foldl( fun check_index/2, Map2, Map1VIndex ) of
        {error,Reason} -> {error,{ErrType,Reason}}; %only propogate errors.
        _ -> true
    end.

%% @hidden
%% @doc Wrap checking if a term is a key in the map with error handling.
check_index( MaybeKey, Map ) ->
    case maps:is_key( MaybeKey, Map ) of
       true -> Map;
       false -> {error, MaybeKey}
    end.

