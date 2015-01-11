%% Registered name for the first broker event message queue, to which all
%% plugins will publish unless overridden by broker specifications. 
-define(BROKER_EVENT_SINK, libemp_event_sink).

%% Unifiers are named after the plugins for which they work.
-define(PLUGIN_UNIFIER( PlugName ), 
        libemp_util:concat_atoms(['unifier_', PlugName])).

