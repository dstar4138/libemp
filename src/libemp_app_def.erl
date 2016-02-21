%%% LibEMP Application Definition -
%%%
%%%   LibEMP Applications can be stacked on top of each other so as to reduce
%%%   duplication of work. While the current wiring language may change, the
%%%   API of the application definition will change less frequently.
%%%
-module(libemp_app_def).

%% API
-export([
  new/0,
  add_buffer/4,
  add_monitor/4, add_monitor/5,
  add_stack/2, add_stack/3, add_stack/4,
  validate_wiring/1
]).
-export([
  foldl_buffers/3,
  foldl_monitors/3,
  foldl_processors/3
]).

%% Hacky Type definition to get Dialyzer to recognize the recursive type def.
-type stack_config_opt()  :: {module(),[term()]} | {module(),[term()],fun()}.
-type sub_stack_configs() :: [stack_configs()].
-type stack_configs()     :: [stack_config_opt() | sub_stack_configs()].

%% When the parse is complete we will have an Application definition.
-opaque app_def() :: #{
  monitors => #{ term() => { term(), module(), term() } },
  buffers  => #{ term() => { module(), [term()] } },
  procs    => [ { default|term(), module(), [term()] } ]
  }.
-define(NEW_APP, #{ monitors => #{},
                    buffers => #{},
                    procs => []
                  }).
-export_type([app_def/0]).

%% @doc Create a new Application Definition.
-spec new() -> app_def().
new() -> ?NEW_APP.

%% @doc Add a Monitor definition to the application definition. If the Name of
%%    the monitor is already taken, this will throw an error. This function will
%%    not guarantee validation of the Monitor/Buffer link as the buffer may not
%%    be defined prior to the Monitor (and this is not a requirement).
%% @end
-spec add_monitor(term(),module(),[term()],term(),app_def()) -> app_def().
add_monitor( Name, Module, Configs, BufferName, #{monitors := Mons} = App ) ->
  NewMons = Mons#{Name => {BufferName, Module, Configs}},
  App#{monitors => NewMons}.

%% @doc Does the same as `add_monitor/5', except defaults the Monitor to push to
%%    the default Buffer.
%% @end
-spec add_monitor(term(),module(),[term()],app_def()) -> app_def().
add_monitor( Name, Module, Configs, App ) ->
  add_monitor( Name, Module, Configs, default, App ).

%% @doc Add a Buffer definition to the application definition.
-spec add_buffer(term(),module(),[term()],app_def()) -> app_def().
add_buffer( Name, Module, Configs, #{buffers := Buffs} = App ) ->
  NewBuffs = Buffs#{Name => {Module, Configs}},
  App#{buffers => NewBuffs}.

%% @doc Add a stack to the Application based on module definitions.
-spec add_stack(default|term(),default|fun(),stack_configs(),app_def()) -> app_def().
add_stack( BufferName, DefaultHandler, StackConfigs, #{procs:=Procs}=App ) ->
  {SinkModule,SinkConfigs} = parse_stack( DefaultHandler, StackConfigs ),
  NewProcs = [ {BufferName, SinkModule, SinkConfigs, DefaultHandler} | Procs ],
  App#{procs => NewProcs}.

%% @doc Same as the `add_stack/4' but uses the default Buffer.
-spec add_stack(default|fun(), stack_configs(), app_def()) -> app_def().
add_stack( FaultHandler, StackConfigs, App ) ->
  add_stack( default, FaultHandler, StackConfigs, App ).

%% @doc Same as the `add_stack/4' but uses the default Buffer and Fault Handler.
-spec add_stack( stack_configs(), app_def() ) -> app_def().
add_stack( StackConfigs, App ) ->
  add_stack( default, default, StackConfigs, App ).

%% @doc Validate that the Application correctly wires Monitors/Sinks to known
%%   and defined Buffers.
%% @end
-spec validate_wiring( app_def() ) -> ok | {error,{missing_buffer,term()}}.
validate_wiring( #{buffers:=B, monitors:=M, procs:=P} ) ->
  Buffs = maps:keys( B ),
  case validate_monitors( Buffs, M ) of
    ok  -> validate_processors( Buffs, P );
    Err -> Err
  end.

%% @doc Fold over the list of Buffers and perform some task for each. Will
%%   escape early if the function EXITs or returns a value of `{error,_}'.
%% @end
-spec foldl_buffers( fun((BufferItem,App)->App), App, app_def() ) -> App
        when BufferItem :: { Name    :: term(),
                             Module  :: module(),
                             Configs :: [term()]
                           },
             App :: any().
foldl_buffers( Fun, Init, #{buffers := Buffers} ) ->
  BufferList = buffers_to_list( Buffers ),
  libemp_util:escaping_foldl( Fun, Init, BufferList ).

%% @doc Fold over the list of Monitors and perform some task for each. Will
%%   escape early if the function EXITs or returns a value of `{error,_}'.
%% @end
-spec foldl_monitors( fun((MonitorItem,App)->App), App, app_def() ) -> App
          when MonitorItem :: { Name    :: term(),
                                Module  :: module(),
                                Configs :: [term()],
                                BufRef  :: term()
                              },
               App :: any().
foldl_monitors( Fun, Init, #{monitors := Monitors} ) ->
  MonitorList = monitors_to_list( Monitors ),
  libemp_util:escaping_foldl( Fun, Init, MonitorList ).

%% @doc Fold over the list of Processors and perform some task for each. Will
%%   escape early if the function EXITs or returns a value of `{error,_}'.
%% @end
-spec foldl_processors( fun((ProcessorItem,App)->App), App, app_def() ) -> App
          when ProcessorItem :: { BufRef :: term(),
                                  Module :: module(),
                                  Configs :: [term()],
                                  DefaultHandler :: fun()
                                },
               App :: any().
foldl_processors( Fun, Init, #{procs := Processors} ) ->
  ProcessorList = processors_to_list( Processors ),
  libemp_util:escaping_foldl( Fun, Init, ProcessorList ).

%%% =======================================================================
%%% Internal Functionality
%%% =======================================================================

%% @hidden
%% @doc Convert the Stack configuration into a Sink Configuration. Namely, if
%%   stacks are converted into libemp_stack_sink definitions, or the stack is
%%   unwrapped if they are only a single sink without fault handling definition.
%% @end
parse_stack( DefaultHandler, StackConfigs ) ->
  case parse_stack( DefaultHandler, StackConfigs, [] ) of
    [{_,_}=SingleSink] ->
      SingleSink; %Forget the Stack overhead, we didn't override Fault Handling.
    SinkList ->
      to_stack( DefaultHandler, SinkList )
  end.
parse_stack( _, [], ReversedStack ) ->
  lists:reverse( ReversedStack );
parse_stack( DefaultHandler, [Config|Rest], RStack ) ->
  SinkItem = parse_stack_item( DefaultHandler, Config ),
  parse_stack( DefaultHandler, Rest, [SinkItem|RStack] ).
parse_stack_item( DefaultHandler, SubStack ) when is_list( SubStack ) ->
  % Stack Items can be Stacks, which mean hierarchies of stacks.
  parse_stack( DefaultHandler, SubStack );
parse_stack_item( _DefaultHandler, {_,_,_}=Item ) -> Item;
parse_stack_item( _DefaultHandler, {_,_}=Item ) -> Item.
handler( default ) -> [];
handler( Fun ) when is_function( Fun ) -> [{fault_handler, Fun}].
to_stack( Handler, SinkList ) ->
  % To counter-act potential pruning of the list, we add it now.
  Stack = case is_list(SinkList) of true -> SinkList; false -> [SinkList] end,
  {libemp_stack_sink, [ {stack, Stack} | handler( Handler ) ]}.

%% @hidden
%% @doc Validate the list of Monitors that they point to existing buffers.
validate_monitors( Buffers, Monitors ) ->
  validate_tuples( Buffers, maps:values( Monitors ) ).

%% @hidden
%% @doc Validate the list of Processors that they point to existing buffers.
validate_processors( Buffers, Procs ) ->
  validate_tuples( Buffers, Procs ).

%% @hidden
%% @doc Validates a list of tuples where the first element is the buffer name.
validate_tuples( Buffers, List ) ->
  libemp_util:escaping_foldl(
    fun(Tuple, ok) ->
          BufferName = element( 1, Tuple ),
          case lists:member(BufferName, Buffers) of
            true  -> ok;
            false -> {error,{missing_buffer,BufferName}}
          end
    end, ok, List ).

%% @hidden
%% @doc Convert the map of Buffer definitions into a list for processing.
buffers_to_list( BufferMap ) ->
  lists:foldl( fun({BufferName,{Module,Configs}},List) ->
                  [{BufferName,Module,Configs}|List]
               end, [], maps:to_list(BufferMap) ).

%% @hidden
%% @doc Convert the map of Monitor definitions into a list for processing
monitors_to_list( MonitorMap ) ->
  lists:foldl( fun({MonitorName,{BufRef,Module,Configs}},List) ->
                  [{MonitorName,Module,Configs,BufRef}|List]
               end, [], maps:to_list(MonitorMap) ).

%% @hidden
%% @doc Convert the list of processors into a conforming list for processing.
processors_to_list( Processors ) ->
  Processors.

