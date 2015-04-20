-module(libemp_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

%% Helper macro for declaring children of supervisor
-define( CHILD(Name, App, Args), 
            {Name, {App, start, [normal, Args]}, 
                permanent, 20000, supervisor, dynamic} ).

%% ====================================================================
%% External functions
%% ====================================================================

%% @doc Start the primary supervisor for the libemp daemon. This will
%%   initialize the whole libemp subsystem too.
%% @end
start_link( StartArgs ) ->
    process_flag( trap_exit, true ),
    supervisor:start_link( {global, ?MODULE}, ?MODULE, StartArgs ).

%% ====================================================================
%% Server functions
%% ====================================================================

%% @private
%% @doc Initialize the libemp subsystem.
init( _Args ) ->
    {ok, {{one_for_one, 5, 60},
          [ 
            %TODO: Start the LibEMP subsystem.
          ]}}.
