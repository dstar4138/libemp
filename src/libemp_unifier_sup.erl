%%%
%%% The Unification supervisor for a particular plug-in.
%%%
-module(libemp_unifier_sup).
-behaviour(supervisor).

-include("libemp.hrl").
-include("logging.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Mod, Args, Timeout, Type), 
        {Mod, {Mod, start_link, Args}, permanent, Timeout, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Start the Unifier Supervisor subtree for a particular plugin. 
%%  Note this plugin must already be 'installed' otherwise the unifier
%%  module will not have been built and this function will fail.
%% @end
-spec start_link( atom() ) -> {ok, pid()} | ignore | {error, any()}.
start_link( PlugName ) ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, PlugName).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Starts the unifier supervisor subtree. This entails starting the
%%   ETS FIFO buffer which acts as a global event sink for the subscribed
%%   events for a particular plugin. Then it starts a supervisor which 
%%   monitors a dynamic set of unifiers for bottelnecks or failures.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
init( PlugName ) ->
    %TODO: Lookup plugin and load unifier module to vm.
    BufferName = ?PLUGIN_UNIFIER( PlugName ),
    {ok, {{one_for_one, 5, 10}, [ 
        ?CHILD( libemp_ets_fifo_buffer, [ BufferName ], 10000, worker ),
        ?CHILD( libemp_unifier_group_sup, [ PlugName ], 5000, supervisor )
    ]}}.

