%%% LibEMP Wrapper Module -
%%%
-module(libemp).

%% API
-export([
    is_node_up/0,
    start/0, start/2,
    stop/0, stop/1
]).
-export([which_applications/0]).
-export([wire/2, load/1]).

-define(APP, ?MODULE).

%%% =======================================================================
%%% Public API
%%% =======================================================================

%% @doc Check if the LibEMP application is running already.
-spec is_node_up() -> boolean().
is_node_up() -> lists:keymember( ?APP, 1, application:which_applications() ).

%% @doc Load files or raw configs and parse them into an application definition.
load( FilePathsOrRawConfigs ) ->
  Contents = try lists:foldl( fun load_item/2, [], FilePathsOrRawConfigs )
             catch _:_ -> load_item( FilePathsOrRawConfigs, [] ) end,
  libemp_wire:parse(Contents).
load_item( Item, Configs ) when is_tuple( Item ) -> [Item|Configs];
load_item( Item, Configs ) when is_list( Item )  ->
  {ok, Contents} = case filename:extension( Item ) of
                     ".wire" -> file:consult( Item );
                     ".wire_script" -> file:script( Item )
                   end,
  Contents ++ Configs.

%% @doc The same as doing a load/1 then a start/2. It attempts to load your
%%    configurations and then start them under the name of a configuration.
%% @end
wire( AppName, FilePathsOrRawConfigs ) ->
  {ok, App} = libemp:load( FilePathsOrRawConfigs ),
  libemp:start( AppName, App ).

%% @doc Helper function for getting a list of the LibEMP applications which are
%%   loaded and running.
%% @end
-spec which_applications() -> [ atom() ].
which_applications() -> libemp_node:get_applications().

%% @doc Start up the LibEMP Application.
-spec start() -> ok | {error, term()}.
start() -> application:start( ?APP ).

%% @doc Validates that LibEMP is up and running with the given config. If not
%%   it will inject the new application into the running service. Namely,
%%   installing a new app on the already running node. This is the recommended
%%   way to start a LibEMP based application.
%% @end
-spec start( atom(), libemp_wire:app_def() ) -> ok | {error, term()}.
start( AppName, AppDef ) ->
    application:ensure_started( ?APP ),
    case validate_app( AppName, AppDef ) of
        ok    -> libemp_node:inject( AppName, AppDef );
        Error -> Error
    end.

%% @doc Stop the whole LibEMP Application.
-spec stop() -> ok | {error, term()}.
stop() ->
    libemp_node:stop_all_applications(),
    application:stop( ?APP ).

%% @doc Stop just the application listed.
-spec stop( AppName :: atom() ) -> ok | {error, term()}.
stop( AppName ) ->
  libemp_node:stop_application( AppName ).

%%%===================================================================
%%% Internal functions
%%%===================================================================

validate_app( AppName, AppDef ) ->
  case lists:member( AppName, libemp:which_applications() ) of
    true -> {error, {alread_started, AppName}};
    false -> validate_app_def( AppDef )
  end.

validate_app_def( AppDef ) ->
  case libemp_app_def:validate_wiring( AppDef ) of
    ok -> validate_modules_exist( AppDef );
    Err -> Err
  end.

validate_modules_exist( AppDef ) ->
  libemp_app_def:foldl_buffers(
    fun( {_,Module,_}, _ ) -> module_exists(Module) end, ok, AppDef ),
  libemp_app_def:foldl_monitor(
    fun( {_,Module,_,_}, _ ) -> module_exists(Module) end, ok, AppDef ),
  libemp_app_def:foldl_processors(
    fun( {_,Module,_}, _ ) -> module_exists(Module) end, ok, AppDef ).

module_exists( Module ) ->
  case code:ensure_loaded( Module ) of
    {module, _} -> ok;
    {error, Reason} -> throw( {Reason, Module} )
  end.
