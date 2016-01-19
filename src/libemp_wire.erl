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
-export_type([config_item/0, app_def/0]).

%% Potential configuration item that can be found in a wiring file:
-type config_item() ::
    {buffer | monitor, module(), [term()]} |
    {buffer | monitor | sink, term(), module(), [term()]} |
    {stack, [ term() | [ term() ] | {module(), [term()]} ]} |
    {stack, [ term() | [ term() ] | {module(), [term()]} ], term()} |
    {stack, [ term() | [ term() ] | {module(), [term()]} ], term(),
      libemp_stack:fault_handler()}.

%% When the parse is complete we will have an Application definition.
-type app_def() :: #{
  monitors => #{ term() => {module(), term()}},
  buffer   => default | { term(), module(), [term()] },
  stacks   => [
    [ {module(),[term()]} ]
  ]}.
-define(NEW_APP, #{monitors=>#{}, buffer=>default, stacks=>[]}).

%%% =======================================================================
%%% Public API
%%% =======================================================================

%% @doc Parse out the wiring configurations into an Application Definition.
%%   This can be used to inject onto a LibEMP Node.
%% @end
-spec parse( [config_item()] ) -> {ok, app_def()} | {error, any()}.
parse( Configs ) ->
  parse( Configs, ?NEW_APP ).
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
m_buffer( Name, Module, Configs, #{buffer := Buffer}=App ) ->
  case Buffer of
    default -> % Safe to override.
      App#{buffer => {Name,Module,Configs}};
    _ -> % Not safe to override. Only one buffer per 'app'.
      {error, too_many_buffers}
  end.

%% @hidden
%% @doc Abstract over the Application Monitor merges. This allows us to have a
%%    consistent error mechanism.
%% @end
m_monitor( Name, Module, Configs, #{monitors := Mons}=App ) ->
  case maps:is_key(Name, Mons) of
    true -> {error, {duplicate_monitor, Name}};
    false ->
      NewMons = Mons#{Name => {Module,Configs}},
      App#{monitors => NewMons}
  end.

%% @hidden
%% @doc Abstract over the Application Stack merges. This allow us to have a
%%    consistent error mechanism.
%% @end
m_stack( [], _FaultHandler, _Sinks, _App) ->
  {error, empty_stack};
m_stack( StackDef, FaultHandler, Sinks, #{stacks := Stacks}=App ) ->
  case create_stack( StackDef, FaultHandler, Sinks ) of
    {ok, Stack} ->
      NewStacks = [Stack|Stacks],
      App#{stacks=>NewStacks};
    Error -> Error
  end.

%% @hidden
%% @doc Create a stack based on the given Stack definition (and the Sink
%%    definitions), and potentially a fault-handler function.
%% @end
create_stack( StackDef, FaultHandler, Sinks ) ->
  {ok, Stack} = loop_lookup( StackDef, Sinks, [] ),
  {ok, [ {stack, Stack} | handler(FaultHandler) ]}.

%% @hidden
%% @doc Validate and pass in the fault function given via the
handler(default) -> [];
handler(Fun) when is_function( Fun ) -> [{fault_handler,Fun}].

%% @hidden
%% @doc For each Sink in a Stack Definition, if it just a reference, grab it
%%    from the Sink definitions and replace it.
%% @end
loop_lookup( [], _, Stack ) -> {ok, lists:reverse(Stack)};
loop_lookup( [ List | Rest ], Sinks, Stack ) when is_list( List ) ->
  case loop_lookup( List, Sinks, [] ) of % Recurse, to allow for sub-stacks.
    {ok, SubStack} ->
      % Wrap sub-stacks as 'libemp_stack_sink' sinks.
      NewStack = [{libemp_stack_sink, [{stack,SubStack}]}|Stack],
      loop_lookup( Rest, Sinks, NewStack );
    _ -> % Attempt to use "List" as a "Name" before conceding defeat.
      (case maps:find(List, Sinks) of
         {ok, Sink} -> loop_lookup(Rest, Sinks, [Sink|Stack]);
         _ -> {error, {missing_sink, List}}
       end)
  end;
loop_lookup( [{_,_}=Sink|Rest], Sinks, Stack ) ->
  loop_lookup( Rest, Sinks, [Sink|Stack] );
loop_lookup( [Name|Rest], Sinks, Stack ) ->
  case maps:find(Name, Sinks) of
    {ok, Sink} -> loop_lookup(Rest, Sinks, [Sink|Stack]);
    _ -> {error, {missing_sink,Name}}
  end.

