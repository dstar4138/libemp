%%% LibEMP Buffer Supervisor - 
%%%
%%%   Configure, Load, and Supervise the local buffer. This buffer can
%%%   be a networked buffer itself, but the implementation is ignored
%%%   and so this supervisor is local only. We instead rely on alternate
%%%   forms of distribution.
%%%
-module(libemp_buffer_sup).
-behaviour(supervisor).
-include("internal.hrl").

%% API
-export([
  start_link/0,
  add_buffer/3, remove_buffer/3
]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor by reading the application's environment 
%%   for buffer configurations, this is done by default by starting up
%%   LibEMP.
%% @end
start_link() ->
    supervisor:start_link( {local, ?MODULE}, ?MODULE, [] ).

%% @doc Add a buffer to the supervisor, this will launch the buffer and
%%   tag it as the given name. If the name already exists, this function
%%   will fail.
%% @end
add_buffer(Name, Module, Configs) ->
  case libemp_buffer:validate_configs( Name, Module, Configs ) of
    ok ->
      supervisor:start_child( ?MODULE, [Name,Module,Configs] );
    {error,_}=Error ->
      Error
  end.

%% @doc Terminate the buffer behind the Initializer with the given PID.
remove_buffer( _Reason, Pid, Initializer ) ->
  libemp_buffer:destroy( Initializer ),
  supervisor:terminate_child(?MODULE, Pid).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Initialize the Buffer provided in the buffer args and override
%%   any default configuration provided as well.
%% @end 
init( _ ) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => buffer,
                    restart => transient,
                    start => {libemp_buffer, start_link, []}
    }],
    {ok, {SupFlags, ChildSpecs}}.

