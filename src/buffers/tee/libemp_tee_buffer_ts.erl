%%% LibEMP Tee Buffer Taker Supervisor
%%%
%%%
-module(libemp_tee_buffer_ts).
-behaviour(supervisor).

%% API
-export([start_link/0, new_child/1]).
%% Supervisor callbacks
-export([init/1]).

%% @doc Starts the supervisor
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Starts a Taker Server as a supervised process under this supervisor.
new_child( {_Giver, _Component} = ChildArgs ) ->
  {ok, _Pid} = supervisor:start_child( ?MODULE, [ChildArgs] ).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Initialization of the simple supervisor for all Taker servers.
init([]) ->
  {ok, {{simple_one_for_one, 5, 5}, [
    {ignore, {libemp_tee_buffer_taker, start_link, []},
      permanent, 5000, worker, [libemp_tee_buffer_taker]}
  ]}}.
