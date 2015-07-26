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
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor with the given buffer arguments.
start_link( BufferArgs, SystemArgs ) ->
    supervisor:start_link( {local, ?MODULE}, ?MODULE, [BufferArgs,SystemArgs] ).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Initialize the Buffer provided in the buffer args and override
%%   any default configuration provided as well.
%% @end 
init( [BufferArgs, SystemArgs] ) ->
    Defaults = get_default_configs( BufferArgs ),
    Overrides = lists:keymerge(1, BufferArgs, Defaults ),
    MFA = build_mfa( Overrides, SystemArgs ),
    SupFlags = build_supflags( Overrides ),
    {ok, { SupFlags,  
           [ #{ id => libemp_buffer, start => MFA, type => worker } ]
         }}.

%%%===================================================================
%%% Buffer Setup
%%%===================================================================

%% @hidden
%% @doc Generate the supervisory flags for this buffer. By default, we just
%%   forward the crash upwards which lets the rest of the system crash.
%% @end
build_supflags( Overrides ) ->
    proplists:get_value( buffer_sup_flags, 
                         Overrides, 
                         #{ strategy  => one_for_one,
                            intensity => 0, % No restarts,
                            period    => 1  % within the timeframe of 1 second.
                          }).

%% @hidden
%% @doc Build the MFA, complete with configuration for the start up of the
%%   LibEMP Buffer.
%% @end
build_mfa( Overrides, SystemArgs ) ->
    {libemp_buffer, start_link, [Overrides, SystemArgs]}. 

%% @hidden
%% @doc Get the default Buffer configuration in the default path location,
%%   or with the override queue location.
%% @end
-spec get_default_configs( [term()] ) -> [term()].
get_default_configs( Configs ) ->
    {Module, ExPath} = get_module_path( Configs ), 
    case file:consult( ExPath ) of
        {error, _} -> 
            ?WARN("Unable to find config for '~p' at '~p'", [Module, ExPath]),
            [];
        {ok, Value} ->
            Value
    end.

%% @hidden
%% @doc Get the module atom and the default configuration path.
-spec get_module_path( [term()] ) -> {atom(), string()}.
get_module_path( Configs ) ->
    Buffer = proplists:get_value(buffer_module, Configs, libemp_simple_buffer),
    ExPath = case 
                proplists:get_value( buffer_override_config_path, Configs ) 
             of
                undefined -> get_builtin_path( Buffer );
                Path -> Path
             end,
    {Buffer, ExPath}.

%% @hidden
%% @doc Get the default configuration path of the builtin LibEMP Queue. 
-spec get_builtin_path( atom() ) -> string().
get_builtin_path( Module ) ->
    QueueName = queue_name_from_module( Module ),
    ExPath = lists:flatten(["src/buffers/",QueueName,"/",Module,".cfg"]),
    case filelib:last_modified( ExPath ) of
        0 -> % Does not exist
            NewPath = lists:flatten(["../src/buffers/testing/",Module,".cfg"]),
            case filelib:last_modified(NewPath) of
                0 -> error({badarg,Module});
                _ -> NewPath
            end;
        _ -> ExPath
    end.

%% @hidden
%% @doc Pull out the queue name from the Module name, if it follows the normal
%%   pattern. Otherwise it will raise a match error.
%% @end
-spec queue_name_from_module( atom() ) -> string().
queue_name_from_module( Module ) ->
   [_, QueueName, _] = string:tokens( atom_to_list(Module), "_" ),
   QueueName.


