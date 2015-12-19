%%% Timer Plug-in -
%%%
-module(libemp_timer_plugin).
-behaviour(libemp_plugin).
-vsn({1,0,0}).

-export([register_actions/0, register_events/0,
         query_runnable/1, init/2
        ]).

%% @doc Register the actions this plug-in can accept.
register_actions() -> [].

%% @doc Register the events this plug-in can generate.
register_events() -> [].

%% @doc Check if this plugin can run on this machine.
query_runnable( _SystemArgs ) -> 
    OverrideArgs = [],
    {true, OverrideArgs}.

init(_,_) -> ok.
