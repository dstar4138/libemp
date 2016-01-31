%%% LibEMP Primary Supervisor -
%%% 
%%%   Launch the LibEMP Framework based on the environment settings. See
%%%   Wiring Config for more details on how to update settings. A local 
%%%   Buffer is started first, if it were to suffer fault, we presently need 
%%%   to bring down all sinks and monitors to reconnect them to the live buffer. 
%%%   This is a current known limitation.
%%%
-module(libemp_sup).
-behaviour(supervisor).
-include("internal.hrl").

-export([start_link/0, init/1]).

%% ====================================================================
%% External functions
%% ====================================================================

%% @doc Start the primary supervisor for the libemp daemon. This will
%%   initialize the whole libemp subsystem too.
%% @end
start_link() ->
    supervisor:start_link( {local, ?MODULE}, ?MODULE, [] ).

%% ====================================================================
%% Server functions
%% ====================================================================

%% @private
%% @doc Initialize the libemp subsystem supervisor.
init([]) ->
    process_flag( trap_exit, true ),
    SupFlags = #{ strategy  => rest_for_one, 
                  intensity => 1,
                  period    => 5 
                },
    {ok, {SupFlags, [
            % Node starts first to record local state.
            #{ id    => libemp_node,
               start => {libemp_node, start_link, []},
               type  => worker
             },
            % Buffer starts second to allow for wiring to succeed.
            #{ id    => libemp_buffers, 
               start => {libemp_buffer_sup, start_link, []},
               type  => supervisor 
             },
            % Then Processors start up next so that sinks are ready for 
            % any incoming events Monitors can generate.
            #{ id    => libemp_processors,
               start => {libemp_processor_sup, start_link, []},
               type  => supervisor
             },
            % Finally, monitors start up.
            #{ id    => libemp_monitors,
               start => {libemp_monitor_sup, start_link, []},
               type  => supervisor
             }
     ]}}.

