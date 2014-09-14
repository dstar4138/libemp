-module(libemp).
-behaviour(application).

-include("logging.hrl").
-include("types.hrl").

%% Exported Type Signatures masked by opaques.
-opaque event() :: libemp_event().
-export_type([ event/0 ]).

-export([start/0, stop/0]).
-export([start/2, prep_stop/1, stop/1]).

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
%%   restartable?, stateless?, etc...)
%% @end
-spec install_plugin( [term()] ) -> ok | {error, any()}.
install_plugin( Config ) -> ok.

uninstall_plugin( Name ) -> ok.
pause_plugin( Name ) -> ok.
unpause_plugin( Name ) -> ok.
plugin_list() -> [].


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
                 [ predicate_map() ], 
                 atom(), 
                 atom(), 
                 fun( (libemp:event()) -> [term()] ) 
               ) -> ok | {error, any()}.
subscribe( EventName, 
           Mapping,
           ReceiverPlugin, 
           ReceiverAction,
           ReceiverParameters ) -> ok.

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

