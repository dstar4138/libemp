-module(libemp).
-behaviour(application).

-include("internal.hrl").

%% Exported Type Signatures masked by opaques.
-opaque event() :: libemp_event().
-export_type([ event/0 ]).

%% Broker Service control
-export([start/0, stop/0]).
-export([start/2, prep_stop/1, stop/1]).

%% Plugin control
-export([install_plugin/1, uninstall_plugin/1, 
         pause_plugin/1, unpause_plugin/1,
         list_plugin/0, describe_plugin/1]).

%% Event control
-export([subscribe/5, publish/2]).


%% @doc Start the EMP application.
start() -> 
    ok = application:start(libemp).

%% @doc Stop the EMP application.
stop() ->
    ok = application:stop(libemp).

%% ---------------------------------
%% LibEMP API
%% ---------------------------------

%% @doc Install a plug-in by providing some configuration details, like the
%%   module name, and how EMP should handle the plugin (distributed?, 
%%   restartable?, stateless?, etc...). This is potentially an expensive 
%%   action (to distribute the plugin across the cluster) but should not 
%%   cause recompilation of the broker.
%% @end
-spec install_plugin( [term()] ) -> possible_error( any() ).
install_plugin( Config ) -> ?NOT_IMPLEMENTED( ok ).

%% @doc Uninstall a plug-in by unlinking the code and removing it from the
%%   cluster. Will cause a recompilation if subscriptions were installed for
%%   the plugin uninstalling. Note this removes ALL SAVED DATA FOR THE PLUGIN.
%% @end
-spec uninstall_plugin( plugin_ref() ) -> possible_error( any() ). 
uninstall_plugin( Name ) -> ?NOT_IMPLEMENTED( ok ).

%% @doc Temporary removal of all subscriptions via a recompile and suppression.
%%   All subscriptions are intact but are remove from the broker. Good for 
%%   testing broker changes due to an additional plugin.
%% @end
-spec pause_plugin( plugin_ref() ) -> possible_error( any() ).
pause_plugin( Name ) -> ?NOT_IMPLEMENTED( ok ).

%% @doc Remove the temporary suppression and recompile the broker if 
%%   neccessary. Will reactivate the plugin across the cluster.
%% @end
-spec unpause_plugin( plugin_ref() ) -> possible_error( any() ).
unpause_plugin( Name ) -> ?NOT_IMPLEMENTED( ok ).

%% @doc List all plugins currently installed. This will also include paused
%%   plugins as they have already gone through the installation process.
%% @end
-spec list_plugin() -> success_error( [ PluginDesc ], any() ) 
                            when PluginDesc :: {plugin_name(), plugin_module()}.
list_plugin() -> ?NOT_IMPLEMENTED( [] ).

%% @doc Get the current configuration for the provided plugin reference.
-spec describe_plugin( plugin_ref() ) -> success_error( Description, any() )
                            when Description :: [{ atom(), term() }].
describe_plugin( Name ) -> ?NOT_IMPLEMENTED( [] ).

%% @doc Subscribe a receiving plug-in to a particular event as long as the
%%   event's attributes match a particular set of predicates (content-based 
%%   subscription). This mapping is used to update the unification tree to
%%   trigger the execution of the receiver's Action.
%%
%%  Examples:
%%      subscribe( tick@timer,
%%                 [ {hour, {eq, 0}}, {minute, {eq, 0}} ],
%%                 camera,
%%                 take_still,
%%                 fun(_) -> [] end ).
%%          This will take a picture on the camera plugin at midnight every 
%%          night. Perhaps to aid in tracking stars/satilites or as a security
%%          mechanism.
%%
%%      subscribe( new_image@camera,
%%                 true, 
%%                 nas_backup,
%%                 backup_file,
%%                 fun( Event ) ->
%%                      FromPath = ?ATTR( Event, filepath ),
%%                      ToPath = io_lib:format("camera/stills/%d/.".[?timestamp])
%%                      [FromPath, ToPath]
%%                  end ).
%%          This will copy any new images to the local NAS setup using the
%%          nas_backup plugin. Note the usage of the ATTR definition for 
%%          accessing the event attributes for addition to the 
%% @end      
-spec subscribe( libemp_event_name(), 
                 predicate_mapping(), 
                 plugin_ref(), plugin_action(), Callback
               ) -> possible_error( any() )
               when Callback :: fun( (libemp:event()) -> Args ),
                    Args :: [ term() ].
subscribe( EventName, 
           Mapping,
           ReceiverPlugin, 
           ReceiverAction,
           ReceiverParameters ) -> ?NOT_IMPLEMENTED( ok ).

%% @doc Publish an event with a set of attributes. It is suggested that the
%%   publisher have a default set of attributes for each event, so that there 
%%   is no delay in publishing.
%% @end 
-spec publish( pid(), libemp_event_name(), libemp_event_attr() ) -> 
    libemp:event().
publish( EventName, Attributes ) ->
    libemp_brokerage ! {event, EventName, Attributes}.
publish( Broker, EventName, Attributes ) -> 
    try 
        Broker ! {event, EventName, Attributes}
    catch % If provided broker causes an error , send to global registry. 
        _:_ -> publish( EventName, Attributes )  
    end.




%% ===================================================================
%% Application callbacks
%% ===================================================================

%% @doc Called by `application` module to start up the libemp Erlang 
%%  OTP supervision tree.
%% @end
start(_StartType, StartArgs) -> 
    libemp_sup:start_link( StartArgs ). % start daemon

%% @doc Called right before stopping libemp.
prep_stop( _State ) -> 
    ?LOG("libemp stopping").

%% @doc Called after stopping libemp. Unused.
stop(_State) -> 
    ok.

