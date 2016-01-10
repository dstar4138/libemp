%%% LibEMP Monitor Behaviour and Object Wrapper -
%%%
%%%     This module defines the structure of an EMP Monitor such that LibEMP
%%%     knows how to comunicate with it and vice-versa. Monitors control its
%%%     own state for therefore there is no stateful functionality currently.
%%%     However, we will link the Monitor's Pid to LibEMP's supervision tree
%%%     to allow for error handling.
%%%
%%%     Note: This is a first draft version of the Monitor Behaviour. It should
%%%       really also take into account many of the fault-tolerance mechanisms
%%%       brought up in pre-planning. The below is an MVP to get Monitors
%%%       implemented and pushing to buffers. We can itterate later.
%%%
-module(libemp_monitor).
-include("../internal.hrl").

-export([start_link/3, start/3]).
-export([stop/1]).
-export([get_monitor/1]).

-opaque platform_reference() :: #monitorref{}.
-export_type([platform_reference/0]).

%%% Describe the Monitor to LibEMP.
%%%   We do this before initializing or even registering to see if the Monitor
%%%   can run on the local host. If it fails or returns ignore, LibEMP will not
%%%   try to initialize. Additionally, return the Configuration to pass into the
%%%   initialization step.
-callback describe( MonitorArgs ) -> {ok, MonitorName, MonitorConfig}
                            | ignore
                            | {error, Reason}
    when MonitorArgs   :: [ term() ],
         MonitorName   :: string() | atom(),
         MonitorConfig :: [ monitor_config() ],
         Reason        :: term().

%%% Initialize the Monitor State:
%%%   Start up the Monitor and give it the Refrence to LibEMP. It can use this
%%%   reference via calls to `libemp_monitor_api' to interact with LibEMP from
%%%   from inside the Monitor implementation.
-callback setup( MonitorArgs, MonitorConfig, PlatformReference ) ->
              {ok, Pid} | ignore | {error, Reason}
    when MonitorArgs       :: [ term() ],
         MonitorConfig     :: [ monitor_config() ],
         PlatformReference :: platform_reference(),
         Pid               :: pid(),
         Reason            :: term().

%% @doc Start the Monitor and link it to the Buffer and supervision tree.
start_link( Module, MonitorArgs, LinkedBufferName ) ->
  case start( Module, MonitorArgs, LinkedBufferName ) of
    {ok, Pid}=Ref -> link(Pid), Ref;
    Otherwise     -> Otherwise
  end.

%% @doc Start the Monitor and link it to the Buffer.
-spec start( module(), [term()], atom() | libemp_buffer:libemp_buffer_init()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: any()}.
start( Module, MonitorArgs, Buffer ) ->
  case do_describe( Module, MonitorArgs ) of
    ignore -> ignore;
    {error,_}=E -> E;
    {ok, MonitorName, MonitorConfig} ->
      load( Module, MonitorName, MonitorArgs, MonitorConfig, Buffer )
  end.

%% @doc Get the Monitor's PID and PlatformReference given its name.
get_monitor( _Name ) ->
  %% TODO: get from node the monitor loaded with the name.
  ?NOT_IMPLEMENTED( {error, {badarg, _Name}} ).

%% @doc Stop the Monitor given its
stop( _Name ) ->
  %% TODO: Not sure how I want to terminate/stop the Monitor. The Monitor can
  %%   stop itself, and either way we need to unregister the Monitor from the
  %%   Buffer it is linked to.
  ?NOT_IMPLEMENTED( ok ).

%%% ==========================================================================
%%% Internal Functionality
%%% ==========================================================================

%% @hidden
%% @doc Trigger the monitor load, by creating a platform reference and passing
%%    it through.
%% @end
load( Module, Name, Args, Config, Buffer ) ->
  case create_ref( Name, Module, Args, Config, Buffer ) of
    {error, _}=Error -> Error;
    {ok, PlatformReference} ->
      case do_setup( Module, Args, Config, PlatformReference ) of
        {ok, Pid} ->
          PR = PlatformReference#monitorref{pid=Pid,linked=self()},
          libemp_node:save_monitor(Name, PR),
          {ok, Pid};
        Otherwise -> Otherwise
      end
  end.

%% @hidden
%% @doc Create an opaque Platform Reference to hand off to the Monitor
%%   Behaviour implementation.
%% @end
create_ref( Name, Module, Args, Config, Buffer ) ->
  case libemp_buffer:register( give, Buffer ) of
    {ok, BufferRef} ->
        Ref = #monitorref{ name = Name,
                           module = Module,
                           args = Args,
                           config = Config,
                           buffer = BufferRef
                         },
        {ok, Ref};
    Otherwise -> Otherwise
  end.

%% @hidden
%% @doc Wrap calls to the behaviour's implementation of `describe/1'.
do_describe( Module, MonitorArgs ) ->
  Args = [ MonitorArgs ],
  case catch libemp_util:wrap_extern( Module, describe, Args ) of
    {'EXIT',Reason} -> {error,Reason};
    Otherwise       -> Otherwise
  end.

%% @hidden
%% @doc Wrap calls to the behaviour's implementation of `setup/3'.
do_setup( Module, ModuleArgs, ModuleConfig, PlatformReference ) ->
  Args = [ ModuleArgs, ModuleConfig, PlatformReference ],
  case catch libemp_util:wrap_extern( Module, setup, Args ) of
    {'EXIT', Reason} -> {error, Reason};
    Otherwise        -> Otherwise
  end.