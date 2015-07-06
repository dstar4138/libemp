%%% LibEMP Primary Supervisor -
%%% 
%%%   Launch the LibEMP Framework based on the environment settings. See
%%%   libemp.app.src for more details. The local Buffer is started first,
%%%   if it were to suffer fault, we presently need to bring down all sinks 
%%%   and monitors to reconnect them to the live buffer. This is a current
%%%   limitation.
%%%
-module(libemp_sup).
-behaviour(supervisor).
-include("internal.hrl").

-export([start_link/1, init/1]).

%% ====================================================================
%% External functions
%% ====================================================================

%% @doc Start the primary supervisor for the libemp daemon. This will
%%   initialize the whole libemp subsystem too.
%% @end
start_link( StartArgs ) ->
    supervisor:start_link( {local, ?MODULE}, ?MODULE, StartArgs ).

%% ====================================================================
%% Server functions
%% ====================================================================

%% @private
%% @doc Initialize the libemp subsystem using the passed in StartArgs.
init( StartArgs ) ->
    SupFlags = #{ strategy  => rest_for_one, 
                  intensity => 1,
                  period    => 5 
                },
    {ok, {SupFlags, [
            #{ id    => libemp_buffer, 
               start => {libemp_buffer_sup, start_link, [StartArgs]},
               type  => worker 
             },
            #{ id    => libemp_sinks,
               start => {libemp_sink_sup, start_link, [StartArgs]},
               type  => supervisor 
             },
            #{ id    => libemp_monitors,
               start => {libemp_monitor_sup, start_link, [StartArgs]},
               type  => supervisor 
             }
    ]}}.

