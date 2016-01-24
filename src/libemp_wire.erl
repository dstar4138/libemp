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

%% Potential configuration item that can be found in a wiring file:
-type config_item() ::
    {buffer | monitor, module(), [term()]} |
    {buffer | monitor | sink, term(), module(), [term()]} |
    {stack, [ term() | [ term() ] | {module(), [term()]} ]} |
    {stack, [ term() | [ term() ] | {module(), [term()]} ], term()} |
    {stack, [ term() | [ term() ] | {module(), [term()]} ], term(),
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
      Merge = fun(Config,AppObj) -> m(Config,Sinks,AppObj) end,
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
pull_out_sinks( [{sink,Name,Module,Configs}|Rest], {S,D} ) ->
  case maps:is_key(Name,S) of
    true  -> {error, {duplicate_sink,Name}};
    false ->
      NS = S#{Name=>{Module,Configs}},
      pull_out_sinks( Rest, {NS,D} )
  end;
pull_out_sinks( [Def|Rest], {S,D} ) ->
  pull_out_sinks( Rest, {S,[Def|D]} ).

%% @hidden
%% @doc Merge in one of the definitions into the Application.
m(_, _, {error, _}=Error) -> Error;
m({stack,StackDef}, Sinks,         App) -> m_stack(StackDef,default,Sinks,App);
m({stack,StackDef,FH}, Sinks,      App) -> m_stack(StackDef,FH,Sinks,App);
m({buffer,Module,Configs}, _,      App) -> m_buffer(Module,Module,Configs,App);
m({buffer,Name,Module,Configs}, _, App) -> m_buffer(Name,Module,Configs,App);
m({monitor,Module,Configs}, _,     App) -> m_monitor(Module,Module,Configs,App);
m({monitor,Name,Module,Configs}, _, App) -> m_monitor(Name,Module,Configs,App);
m(Unknown, _, _) -> {error, {badarg,Unknown}}.

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
m_monitor( Name, Module, Configs, App ) ->
  libemp_app_def:add_monitor( Name, Module, Configs, App ).

%% @hidden
%% @doc Abstract over the Application Stack merges. This allow us to have a
%%    consistent error mechanism.
%% @end
m_stack( [], _FaultHandler, _Sinks, _App) ->
  {error, empty_stack};
m_stack( StackDef, FaultHandler, Sinks, App ) ->
  case sink_substitution( StackDef, Sinks, [] ) of
    {ok, Stack} -> libemp_app_def:add_stack( FaultHandler, Stack, App );
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


