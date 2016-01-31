%%% LibEMP Monitor Supervisor - 
%%%

-module(libemp_monitor_sup).
-behaviour(supervisor).

%% API
-export([
  start_link/0,
  add_monitor/4, remove_monitor/1
]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Adds a monitor to the supervisor and starts it up.
add_monitor(Name, Module, MonitorArgs, LinkedBufferName) ->
  supervisor:start_child( ?MODULE, [Name,Module,MonitorArgs,LinkedBufferName]).

%% @doc Requires the Process ID to stop the child, but will safely tell the
%%   Monitor to destroy itself by contacting it's signal trapper.
%% @end
remove_monitor( Pid ) when is_pid( Pid ) ->
  supervisor:terminate_child( ?MODULE, Pid );
remove_monitor( Name ) ->
  case libemp_node:get_monitor( Name ) of
    {ok, Pid, _} ->
      supervisor:terminate_child( ?MODULE, Pid );
    Err -> Err
  end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Initialize each Monitor separately.
init( _ ) ->
    SupFlags = #{
      strategy => simple_one_for_one, % Each can be restarted independently
      interval => 5,                  % Allow up to 5 failed attempts within,
      period   => 5                   % 5 seconds before giving up.
    },
    ChildSpecs = [#{id=>monitor,
                    restart => transient,
                    start => {libemp_monitor, start_link, []}
    }],
    {ok, {SupFlags, ChildSpecs}}.

