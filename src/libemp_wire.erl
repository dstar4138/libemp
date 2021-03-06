%%% LibEMP Default Wiring Configuration Parsing -
%%%
%%%   There are only four types of configuration; buffers, monitors, sinks, and
%%%   stacks. With these configuration items, we can create an Application.
%%%
%%%   Note: a current perceived limitation of this approach is that an
%%%     Application can only have a single buffer. This makes sense though
%%%     as the only reason why more than buffer could be required is to
%%%     segregate the logical pieces. LibEMP wants to make this segregation
%%%     more apparent.
%%%
-module(libemp_wire).

-export([ parse/1 ]).
-export_type([config_item/0]).
-export_type([component_type/0, component_name/0]).
-export_type([module_config/0, stack_config/0]).

-type component_type() :: buffer | monitor | sink.
-type component_name() :: atom().
-type module_config() :: module() | {module(), [term()]}.
-type stack_config() :: [ component_name() | module_config() | stack_config() ].

%%% Potential configuration item that can be found in a wiring file:
-type config_item() ::
      {component_type(), module_config()}
    | {component_type(), component_name(), module_config()}
    | {monitor, component_name(), module_config(), Buffer :: component_name()}
    | {stack, stack_config()}
    | {stack, stack_config(), Buffer :: component_name()}
    | {stack, stack_config(), Buffer :: component_name(),
                                libemp_stack:fault_handler()}.

%%% =======================================================================
%%% Public API
%%% =======================================================================

%% @doc Parse out the wiring configurations into an Application Definition.
%%   This can be used to inject onto a LibEMP Node.
%% @end
-spec parse( [config_item()] ) -> {ok, libemp_app_def:app_def()}
                                | {error, any()}.
parse( Configs ) ->
  parse( Configs, libemp_app_def:new() ).
parse( Configs, App ) ->
  case pull_out_sinks( Configs ) of
    {ok, {Sinks, Definitions}} ->
      Merge = fun(Config,AppObj) -> m( Config, Sinks, AppObj ) end,
      libemp_util:escaping_foldl( Merge, App, Definitions );
    Error -> Error
  end.

%%% =======================================================================
%%% Private API
%%% =======================================================================

%% @hidden
%% @doc Pull out the sinks from the full Config list. We use the Sinks as the
%%    Environment, whereas the rest of the configurations are just the
%%    definitions.
%% @end
pull_out_sinks( Configs ) ->
  pull_out_sinks(Configs,{#{},[]}).
pull_out_sinks( [], Refs ) ->
  {ok, Refs};
pull_out_sinks( [{sink,ModuleConfigs}|Rest], State ) ->
  {Module,Configs} = module_configs(ModuleConfigs),
  append_and_continue_pull_out( Module, Module, Configs, Rest, State );
pull_out_sinks( [{sink,Name,ModuleConfigs}|Rest], State ) ->
  {Module,Configs} = module_configs(ModuleConfigs),
  append_and_continue_pull_out( Name, Module, Configs, Rest, State );
pull_out_sinks( [Def|Rest], {S,D} ) ->
  pull_out_sinks( Rest, {S,[Def|D]} ).
append_and_continue_pull_out( Name, Module, Configs, Rest, {S,D} ) ->
  case maps:is_key(Name,S) of
    true  -> {error, {duplicate_sink,Name}};
    false ->
      NS = S#{Name=>{Module,Configs}},
      pull_out_sinks( Rest, {NS,D} )
  end.

%% @hidden
%% @doc Merge in one of the definitions into the Application.
m(_, _, {error, _}=Error) -> Error;

m({stack, StackDef}, Sinks, App) ->
  m_stack( StackDef, default, default, Sinks, App );
m({stack,StackDef,Buf}, Sinks, App) ->
  m_stack( StackDef, Buf, default, Sinks, App );
m({stack,StackDef,BufferName,FaultHandler}, Sinks, App) ->
  m_stack( StackDef, BufferName, FaultHandler, Sinks, App );

m({buffer,ModuleConfigs}, _, App) ->
  {Module, Configs} = module_configs(ModuleConfigs),
  m_buffer( Module, Module, Configs, App );
m({buffer,Name,ModuleConfigs}, _, App) ->
  {Module, Configs} = module_configs(ModuleConfigs),
  m_buffer( Name, Module, Configs, App );

m({monitor,ModuleConfigs}, _, App) ->
  {Module, Configs} = module_configs(ModuleConfigs),
  m_monitor( Module, Module, Configs, default, App );
m({monitor,Name,ModuleConfigs}, _, App) ->
  {Module, Configs} = module_configs(ModuleConfigs),
  m_monitor( Name, Module, Configs, default, App );
m({monitor,Name,ModuleConfigs,BufferName}, _, App) ->
  {Module,Configs} = module_configs(ModuleConfigs),
  m_monitor( Name, Module, Configs, BufferName, App );

m(Unknown, _, _) -> {error, {badarg,Unknown}}.

%% @hidden
%% @doc Be explicit about no configs even if the wire config writer was not.
module_configs( {_Mod,_Conf}=MC ) -> MC;
module_configs( Mod ) when is_atom( Mod ) -> {Mod,[]}.

%% @hidden
%% @doc Abstract over the Application Buffer merge. This allows us to have a
%%    consistent error mechanism.
%% @end
m_buffer( Name, Module, Configs, App ) ->
  libemp_app_def:add_buffer( Name, Module, Configs, App ).

%% @hidden
%% @doc Abstract over the Application Monitor merges. This allows us to have a
%%    consistent error mechanism.
%% @end
m_monitor( Name, Module, Configs, BufferName, App ) ->
  libemp_app_def:add_monitor( Name, Module, Configs, BufferName, App ).

%% @hidden
%% @doc Abstract over the Application Stack merges. This allow us to have a
%%    consistent error mechanism.
%% @end
m_stack( [], _BufferName, _FaultHandler, _Sinks, _App) ->
  {error, empty_stack};
m_stack( StackDef, BufferName, FaultHandler, Sinks, App ) ->
  case sink_substitution( StackDef, Sinks, [] ) of
    {ok, Stack} -> libemp_app_def:add_stack( BufferName, FaultHandler, Stack, App );
    Error -> Error
  end.

%% @hidden
%% @doc Loop through a stack config and replace all the Sink names with the
%%   previously defined Sink configs.
%% @end
sink_substitution( [], _, Stack ) -> {ok, lists:reverse(Stack)};
sink_substitution( [ Item | Rest ], Sinks, Stack ) ->
  case do_substitution( Item, Sinks ) of
    {error,_}=Error -> Error;
    SItem -> sink_substitution( Rest, Sinks, [SItem|Stack] )
  end.

%% @hidden
%% @doc Given an item's name or a list of items, look in the environment
%%    dictionary and replace the values in place.
%% @end
do_substitution( List, Environment ) when is_list( List ) ->
  case sink_substitution( List, Environment, [] ) of
    {ok, ReplacedList} -> ReplacedList;
    _ -> % Attempt to use the List as a string "Name" before conceding defeat.
      do_item_lookup( List, Environment )
  end;
do_substitution( Item, Environment ) ->
  do_item_lookup( Item, Environment ).
do_item_lookup( Item, Environment ) ->
  case maps:find( Item, Environment ) of
    {ok, Definition} -> Definition;
    _ -> {error, {missing_definition,Item}}
  end.


