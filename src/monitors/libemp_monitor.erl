%%% LibEMP Monitor Behaviour and Object Wrapper -
%%%
%%%     This module defines the structure of an EMP Monitor such that LibEMP
%%%     knows how to communicate with it and vice-versa. Monitors control its
%%%     own state for therefore there is no stateful functionality currently.
%%%     However, we will link the Monitor's Pid to LibEMP's supervision tree
%%%     to allow for error handling.
%%%
%%%     Note: This is a first draft version of the Monitor Behaviour. It should
%%%       really also take into account many of the fault-tolerance mechanisms
%%%       brought up in pre-planning. The below is an MVP to get Monitors
%%%       implemented and pushing to buffers. We can iterate later.
%%%
-module(libemp_monitor).
-include("../internal.hrl").

-export([start_link/3, start_link/4, start/4]).
-export([stop/1]).

-opaque platform_reference() :: #monitorref{}.
-export_type([platform_reference/0]).

%%% Describe the Monitor to LibEMP.
%%%   We do this before initializing or even registering to see if the Monitor
%%%   can run on the local host. If it fails or returns ignore, LibEMP will not
%%%   try to initialize. Additionally, return the Configuration to pass into the
%%%   initialization step.
-callback describe( MonitorName, MonitorArgs ) -> {ok, MonitorConfig}
                            | ignore
                            | {error, Reason}
    when MonitorArgs   :: [ term() ],
         MonitorName   :: term(),
         MonitorConfig :: [ monitor_config() ],
         Reason        :: term().

%%% Initialize the Monitor State:
%%%   Start up the Monitor and give it the Reference to LibEMP. It can use this
%%%   reference via calls to `libemp_monitor_api' to interact with LibEMP from
%%%   from inside the Monitor implementation.
-callback setup( MonitorArgs, MonitorConfig, PlatformReference ) ->
              {ok, NameOrPid} | ignore | {error, Reason}
    when MonitorArgs       :: [ term() ],
         MonitorConfig     :: [ monitor_config() ],
         PlatformReference :: platform_reference(),
         NameOrPid         :: atom() | pid(),
         Reason            :: term().

%%% Uninitialized the Monitor:
%%%   Shutdown the currently running Monitor for the given reason. We pass the
%%%   name or PID returned via the Monitor instance's setup/3 call. This is to
%%%   allow for the
-callback destroy( Reason, NameOrPid, PlatformReference ) -> any()
    when Reason            :: shutdown | term(), % On normal vs abnormal.
         NameOrPid         :: atom() | pid(),
         PlatformReference :: platform_reference().

%%% =======================================================================
%%% Public API
%%% =======================================================================

%% @doc Start the Monitor and link it to the Buffer and supervision tree.
start_link( Module, MonitorArgs, LinkedBufferName ) ->
  start_link( Module, Module, MonitorArgs, LinkedBufferName ).
start_link( MonitorName, Module, MonitorArgs, LinkedBufferName ) ->
  case start( MonitorName, Module, MonitorArgs, LinkedBufferName ) of
    {ok, Pid}=Ref -> link(Pid), Ref;
    Otherwise     -> Otherwise
  end.

%% @doc Start the Monitor and link it to the Buffer.
-spec start( term(), module(), [term()],
                                atom() | libemp_buffer:libemp_buffer_init() ) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: any()}.
start( MonitorName, Module, MonitorArgs, Buffer ) ->
  case libemp_node:get_monitor( MonitorName ) of
    {ok, Pid, Ref} -> {error, {already_started, Pid, Ref}};
    {error, _} ->
      (case do_describe( MonitorName, Module, MonitorArgs ) of
          ignore -> ignore;
          {error,_}=E -> E;
          {ok, MonitorConfig} ->
              load( MonitorName, Module, MonitorArgs, MonitorConfig, Buffer )
       end)
  end.

%% @doc Stop the Monitor given its Platform Reference or the Module/Name of the
%%    Running Monitor.
%% @end
stop( Monitor ) -> stop( shutdown, Monitor ).

%% @doc Stop the Monitor and override it's reason for shutting down.
stop( Reason, #monitorref{module = Module, pid = Pid} = PR ) ->
  libemp_monitor_api:unlink( PR ),
  do_destroy(Module, Reason, Pid, PR);
stop( Reason, ModuleOrName ) ->
  case libemp_node:get_monitor( ModuleOrName ) of
    {ok, _Pid, PR}  -> stop( Reason, PR );
    Otherwise -> Otherwise
  end.

%%% ==========================================================================
%%% Internal Functionality
%%% ==========================================================================

%% @hidden
%% @doc Trigger the monitor load, by creating a platform reference and passing
%%    it through.
%% @end
load( Name, Module, Args, Config, Buffer ) ->
  case create_ref( Name, Module, Args, Config, Buffer ) of
    {error, _}=Error -> Error;
    {ok, PlatformReference} ->
      (case do_setup( Module, Args, Config, PlatformReference ) of
        {ok, Pid} ->
          PR = PlatformReference#monitorref{pid=Pid,linked=self()},
          libemp_node:save_monitor(Name, PR),
          {ok, Pid};
        Otherwise -> Otherwise
      end)
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
do_describe( MonitorName, Module, MonitorArgs ) ->
  Args = [ MonitorName, MonitorArgs ],
  case (catch libemp_util:wrap_extern( Module, describe, Args )) of
    {'EXIT',Reason} -> {error,Reason};
    Otherwise       -> Otherwise
  end.

%% @hidden
%% @doc Wrap calls to the behaviour's implementation of `setup/3'.
do_setup( Module, ModuleArgs, ModuleConfig, PlatformReference ) ->
  Args = [ ModuleArgs, ModuleConfig, PlatformReference ],
  case (catch libemp_util:wrap_extern( Module, setup, Args )) of
    {'EXIT', Reason} -> {error, Reason};
    Otherwise        -> Otherwise
  end.

%% @hidden
%% @doc Wrap calls to the behaviour's implementation of `destroy/3'.
do_destroy( Module, Reason, Pid, PlatformReference ) ->
  Args = [Reason, Pid, PlatformReference],
  case (catch libemp_util:wrap_extern( Module, destroy, Args )) of
    {'EXIT',Error} -> {error, Error};
    Otherwise      -> Otherwise
  end.