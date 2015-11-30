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


