%%% LibEMP Monitor Supervisor - 
%%%

-module(libemp_monitor_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Initialize each Monitor separately.
init( Args ) ->
    ChildSpecs = build_child_specs( Args ),
    SupFlags = #{ 
      strategy => one_for_one, % Each can be restarted independently
      interval => 5,           % Allow up to 5 failed attempts within, 
      period   => 5            % 5 seconds before giving up.
    },
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%% @doc From the override settings and the saved values of previous runs, 
%%  generate the child specs for the Monitors we need to load.
%% @end
build_child_specs( _Args ) -> []. 
