%%% LibEMP External Process Monitor -
%%%
%%%   The easiest mechanism for external process monitoring. It considers each
%%%   line of output a new event. It will not process the output and instead
%%%   leave the sink stack to deserialize if it needs to (i.e. JSON to map).
%%%
-module(libemp_extern_monitor).
-behaviour(libemp_monitor).

%% API
-export([ describe/2, setup/3, destroy/3 ]).

%% LibEMP Monitor Definition.
%% We tag the event to show that it came from a specific extern_monitor.
-define(MONITOR_CONFIG( Name ), [
  {extern_name, Name}
]).

%% Default Erlang-Port Configurations
-define(PORT_CONFIGS, [
  {line, 256}, binary, use_stdio, exit_status, {parallelism, true}
]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Return the default arguments based on the name provided the
%%   user-given arguments are complete.
%% @end
describe( Name, Args ) ->
  case get(command, Args, undefined) of
    undefined ->
      {error, {missing_command, Args}};
    _ ->
      {ok, ?MONITOR_CONFIG( Name )}
  end.

%% @doc Start up a port monitoring server.
setup( Args, Configs, EMP ) ->
  {Name, PortName, PortConfigs} = get_configs(Args, Configs),
  libemp_port_server:start_link(Name, EMP, PortName, PortConfigs).

%% @doc Stop the port server which will in turn send an exit signal to
%%   the command.
%% @end
destroy( Reason, Pid, _EMP ) ->
  libemp_port_server:stop( Reason, Pid ).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%% @doc Merge the default configurations and the override args.
get_configs( Args, Configs ) ->
  OverallConfig = lists:keymerge(1, Args, Configs),
  Name          = get(extern_name, OverallConfig),
  Command       = get(command, OverallConfig),
  PortConfigs   = get(port_settings, OverallConfig, ?PORT_CONFIGS),
  PortName      = {spawn, Command},
  {Name, PortName, PortConfigs}.

%% @hidden
%% @doc Throw an error on missing the values unless a default is given.
get( Key, List, Default ) ->
  proplists:get_value( Key, List, Default ).
get( Key, List ) ->
  case proplists:get_value( Key, List ) of
    undefined -> error({missing, Key});
    Value -> Value
  end.

