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
-export([start_link/0, add_buffer/3]).

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
    case
        supervisor:start_link( {local, ?MODULE}, ?MODULE, [] )
    of
      ignore -> ignore;
      {error,_}=E -> E;
      {ok,_}=Ret ->
        initialize_buffers(),
        Ret
    end.

%% @doc Add a buffer to the supervisor, this will launch the buffer and
%%   tag it as the given name. If the name already exists, this function
%%   will fail.
%% @end
add_buffer(Name, Module, Configs) ->
  case libemp_buffer:validate_config( Name, Module, Configs ) of
    ok ->
      supervisor:start_child( ?MODULE, [Name,Module,Configs] );
    {error,_}=Error ->
      Error
  end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Initialize the Buffer provided in the buffer args and override
%%   any default configuration provided as well.
%% @end 
init( _ ) ->
  %TODO: Pull out shutdown setting from config. Along with intensity/period.
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => buffer,
                    restart => permanent,
                    start => {libemp_buffer, start_link, []},
                    shutdown => brutal_kill}],
    {ok, {SupFlags, ChildSpecs}}.

%% TODO: DEAN: I've commented this out for the time being so we can expirement with AppConfig.
%init([]) ->
%    BufferArgs = libemp_util:get_cfgs( buffers ),
%    init([BufferArgs]);
%init([BufferArgs]) ->
%    Defaults = get_default_configs(),
%    Overrides = lists:keymerge(1, BufferArgs, Defaults ),
%    MFA = build_mfa( Overrides ),
%    SupFlags = build_supflags( Overrides ),
%    {ok, { SupFlags,
%           [ #{ id => libemp_buffer, start => MFA, type => worker } ]
%         }}.

%%%===================================================================
%%% Buffer Setup
%%%===================================================================

%% @hidden
%% @doc Get the list of buffers from the application config when the system
%%   starts up, and trigger the supervisor to start a new buffer.
%% @end
initialize_buffers() ->
  lists:foreach(fun({Name, BufferModule, Configs}) ->
    add_buffer(Name,BufferModule,Configs)
  end, get_buffers()).

%% @hidden
%% @doc Actually get the list of buffers from appconfig and turn it into an
%%    easy list to process.
%% @end
get_buffers() ->
  case libemp_util:get_cfgs(buffers) of
    []  -> [];
    Map when is_map( Map ) ->
      % Wire stores buffers as #{ Name => {Module,Configs} }.
      % We need it as [ {Name,Module,Configs} | ... ] for add_buffer.
      lists:foldl(fun(Key, List) ->
        {Module,Configs}=maps:get(Key, Map),
        [{Key,Module,Configs}|List]
        end,
        [],
        maps:keys(Map)
      )
  end.

%% TODO: Delete all of the following functions as they should not be needed anymore.
%%% @hidden
%%% @doc Generate the supervisory flags for this buffer. By default, we just
%%%   forward the crash upwards which lets the rest of the system crash.
%%% @end
%build_supflags( Overrides ) ->
%    proplists:get_value( buffer_sup_flags,
%                         Overrides,
%                         #{ strategy  => one_for_one,
%                            intensity => 0, % No restarts,
%                            period    => 1  % within the timeframe of 1 second.
%                          }).
%
%%% @hidden
%%% @doc Build the MFA, complete with configuration for the start up of the
%%%   LibEMP Buffer.
%%% @end
%build_mfa( Overrides ) ->
%    {libemp_buffer, start_link, [Overrides]}.
%
%%% @hidden
%%% @doc Get the default Buffer configuration in the default path location,
%%%   or with the override queue location.
%%% @end
%-spec get_default_configs() -> [term()].
%get_default_configs() ->
%    {Module, ExPath} = get_module_path(),
%    case file:consult( ExPath ) of
%        {error, _} ->
%            ?WARN("Unable to find config for '~p' at '~p'", [Module, ExPath]),
%            [];
%        {ok, Value} ->
%            Value
%    end.
%
%%% @hidden
%%% @doc Get the module atom and the default configuration path.
%-spec get_module_path() -> {atom(), string()}.
%get_module_path() ->
%    % Get path, fail if not defined.
%    Buffer = libemp_util:get_cfg(buffers, buffer_module),
%    ExPath = case
%                % Get path optionally, otherwise we can try to find it.
%                libemp_util:get_cfg( buffers,
%                                     buffer_config_path,
%                                     undefined )
%             of
%                undefined -> get_builtin_path( Buffer );
%                Path -> Path
%             end,
%    {Buffer, ExPath}.
%
%%% @hidden
%%% @doc Get the default configuration path of the builtin LibEMP Queue.
%-spec get_builtin_path( atom() ) -> string().
%get_builtin_path( Module ) ->
%    QueueName = queue_name_from_module( Module ),
%    ExPath = lists:flatten(["src/buffers/",QueueName,"/",Module,".cfg"]),
%    case filelib:last_modified( ExPath ) of
%        0 -> % Does not exist, try the next common place (i.e. in ebin).
%            NewPath = lists:flatten(["../src/buffers/testing/",Module,".cfg"]),
%            case filelib:last_modified(NewPath) of
%                0 -> error({badarg,Module});
%                _ -> NewPath
%            end;
%        _ -> ExPath
%    end.
%
%%% @hidden
%%% @doc Pull out the queue name from the Module name, if it follows the normal
%%%   pattern. Otherwise it will raise a match error.
%%% @end
%-spec queue_name_from_module( atom() ) -> string().
%queue_name_from_module( Module ) ->
%   [_, QueueName, _] = string:tokens( atom_to_list(Module), "_" ),
%   QueueName.

