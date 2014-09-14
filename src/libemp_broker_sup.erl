%%%
%%% The Broker Supervisor monitors a set of brokers and a ETS Buffer table.
%%% The buffer is used to provide a single sink for all incoming events 
%%% which are then distributed amongst the brokers extremely quickly. It is 
%%% possible to specify a particular broker from an event publish instead; 
%%% to remove indirection and multiplex by hand, but this isn't recommended.
%%%
-module( libemp_broker_sup ).
-behaviour( supervisor ).

-include("libemp.hrl").
-include("logging.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Mod, Args, Timeout, Type), 
        {Mod, {Mod, start_link, Args}, permanent, Timeout, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Start the global Brokerage supervisory tree to keep track of point
%%   of event multiplexing.
%% @end
-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Start the broker supervisor subtree. This entails starting the
%%   ETS FIFO buffer which acts as a global event sink, the buffer monitor
%%   and then the supervisor responsible for the dynamically spawned 
%%   broker instances which multiplex off the buffer. The buffer monitor
%%   can dynamically spawn/kill brokers for efficiency sake.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
init([]) ->
    {ok, {{one_for_one, 2, 10}, [
     ?CHILD( libemp_ets_fifo_buffer, [?BROKER_EVENT_SINK], 10000, worker ),
     ?CHILD( libemp_broker_group_mon, [], 5000, worker ),
     ?CHILD( libemp_broker_group_sup, [], 5000, supervisor )
    ]}}.

