%%% LibEMP Tee Buffer Master Supervisor -
%%%
%%%     Starts the whole OTP supervision tree for the Tee Buffer.
%%%
-module(libemp_tee_buffer_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).
%% Supervisor callbacks
-export([init/1]).

%% The name of the process which will handle Monitor/Sink registration.
-define(REGISTRATION_KEY, libemp_tee_buffer).

-define(CHILD(Id, Mod, Type, Args),
  {Id, {Mod, start_link, Args}, permanent, 5000, Type, [Mod]}).

%% @doc Start the master supervisor and initialize the OTP subsystem.
start_link( Args ) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Start up the giver, and taker supervisor.
init( Args ) ->
  {ok, {{one_for_all, 5, 10}, [
    ?CHILD(?REGISTRATION_KEY, libemp_tee_buffer_giver, worker, [Args]),
    ?CHILD(libemp_tee_buffer_ts, libemp_tee_buffer_ts, supervisor, [])
  ]}}.
