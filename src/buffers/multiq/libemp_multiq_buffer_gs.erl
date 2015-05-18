%%% LibEMP MultiQ Buffer Giver Supervisor
%%%
%%%
-module(libemp_multiq_buffer_gs).
-behaviour(supervisor).

%% API
-export([start_link/0, new_child/1]).
%% Supervisor callbacks
-export([init/1]).

%% @doc Starts the supervisor
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Starts a Giver Server as a supervised process under this supervisor.
new_child( {_Taker, _Component} = ChildArgs ) ->
    {ok, _Pid} = supervisor:start_child( ?MODULE, [ChildArgs] ).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Initialization of the simple supervisor for all Giver servers.
init([]) -> 
    {ok, {{simple_one_for_one, 5, 5}, [
            {ignore, {libemp_multiq_buffer_giver, start_link, []},
                     permanent, 5000, worker, [libemp_multiq_buffer_giver]}
    ]}}.

