%%% LibEMP Wrapper Module -
%%%
-module(libemp).

%% API
-export([
    ensure_started/1,
    start/1, stop/0
]).
-export([
  is_processing_node/0,
  is_monitoring_node/0,
  is_clustered_node/0
]).

%%% =======================================================================
%%% Public API
%%% =======================================================================

%% @doc Check if the environment defines at least one Sink Stack. 
-spec is_processing_node() -> boolean().
is_processing_node() -> check_env( stacks ) > 0.

%% @doc Check if the environment defines at least one Monitor. 
-spec is_monitoring_node() -> boolean().
is_monitoring_node() -> check_env( monitors ) > 0.

%% @doc Check if the buffer is a 
-spec is_clustered_node() -> boolean().
is_clustered_node()  -> false. %TODO: check with buffers to see if 1 is remote.

%% @doc Check if the LibEMP application is running already.
-spec is_node_running() -> boolean().
is_node_running() ->
    lists:keymember( ?MODULE, 1, applications:which_applications() ).

%% @doc Validates that LibEMP is up and running with the given config. If not
%%   it will inject the new application into the running service. Namely, 
%%   installing a new app on the already running node. This is the recommended
%%   way to start a LibEMP based application.
%% @end
-spec ensure_started( [term()] ) -> ok | {error, term()}.
ensure_started( Config ) ->
    ok = case is_node_running() of
            true  -> inject( Config );
            false -> start( Config )
         end,
    application:ensure_started( ?MODULE ).

%% @doc Start up the LibEMP application given a particular set of configs.
-spec start( [term()] ) -> ok | {error, term()}.
start( [] ) -> application:start( ?MODULE );
start( Config ) ->
    application:load( ?MODULE ),            % Ensure libemp libraries loaded,
    case parse_validate( Config ) of        % Parse given config to App wiring,
        {ok, Wiring} ->
            load_wiring_to_appconfig( Wiring ), % Load into App Config,
            application:start( ?MODULE );   % Start up libemp app with config.
        Error -> Error
    end.

%% @doc Stop the LibEMP application.
-spec stop() -> ok | {error, term()}.
stop() ->
    % TODO: Should we itteratively close Monitors, Sinks then Buffers?
    % TODO: Do we need to push the buffer to disk?
    application:stop( ?MODULE ).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%% @doc Inject new configs onto a running application.
inject( Config ) ->
    case parse_validate( Config ) of
        {ok, NewWiring} ->
            OldWiring = get_wiring_from_appconfig(),
            merge_inject( NewWiring, OldWiring );
        Error -> Error
    end.

%% @hidden
%% @doc Merge the new and old wirings, if there is a naming conflict or other
%%   such error, then it returns an error. Otherwise it will perform do the
%%   injection.
%% @end
merge_inject( NewWiring, OldWiring ) ->
    case libemp_wire:merge( NewWiring, OldWiring ) of
        {ok, CompatibleWirings} ->
            do_inject( NewWiring, CompatibleWirings );
        Error -> Error
    end.     

%% @hidden
%% @doc Does the actual injection of a new wiring, and then loads
%%   the merged wiring into the app-config for easy local queries.
%% @end
do_inject( NewWiring, TotalWiring ) ->
    %TODO: for each new item in the NewWiring, send an update to the node
    % i.e. {new, buffer, BufferConfig}, ...
    load_wiring_to_appconfig( TotalWiring ).

%% @hidden
%% @doc Validate/Parse the configs. The config list can be a string (raw wire
%%   file), a list of strings (a set of file paths, absolute or relative), or
%%   a list of n-ary tuples which denote the Wire configs.
%% @end 
parse_validate( Config ) ->
    case libemp_wire:parse( Config ) of
        {ok, Wiring} ->
            libemp_wire:validate( Wiring );
        Error -> Error
    end.

%% @hidden
%% @doc Load the wiring into appconfig, assumes it has been validated.
load_wiring_to_appconfig( Wiring ) ->
    add_to_env( buffers,    libemp_wiring:get_buffers( Wiring ) ),
    add_to_env( monitors,   libemp_wiring:get_monitors( Wiring ) ),
    add_to_env( sinks,      libemp_wiring:get_sinks( Wiring ) ),
    add_to_env( stacks,     libemp_wiring:get_stacks( Wiring ) ),
    add_to_env( fault_funs, libemp_wiring:get_fault_funs( Wiring ) ).
add_to_env( Par, Val ) -> application:set_env( ?MODULE, Par, Val ).

%% @hidden
%% @doc Opposite of loading the wiring into appconfig, this pulls out what
%%   is there and creates an environment.
%% @end
get_wiring_from_appconfig() ->
    {ok,Buffers}  = get_from_env( buffers, #{} ),
    {ok,Monitors} = get_from_env( monitors, #{} ),
    {ok,Sinks}    = get_from_env( sinks, #{} ),
    {ok,Stacks}   = get_from_env( stacks, [] ),
    {ok,FaultFuns}= get_from_env( fault_funs, #{} ),
    libemp_wire:new_env( Buffers, Monitors, Sinks, Stacks, FaultFuns ).
get_from_env( Par, Default ) -> application:get_env( ?MODULE, Par, Default ).

%% @hidden
%% @doc Get the length of the config list for one of our environment parameters.
check_env( Par ) -> get_length( application:get_env( ?MODULE, Par ) ).
get_length( Map ) when is_map(Map) -> maps:size( Map );
get_length( List ) when is_list(List) -> length( List ).

