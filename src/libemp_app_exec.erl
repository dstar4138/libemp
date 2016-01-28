%%% LibEMP Application Execution -
%%%
%%%   This Module implements the functionality required for injection new
%%%   applications onto a running LibEMP node. This is slightly complicated
%%%   due to the desire to have installation be transactional (i.e. if it
%%%   fails, it gets rolled back).
%%%
-module(libemp_app_exec).

%% API
-export([
  install/1,
  uninstall/2
]).

%% @doc Transactional installation of the LibEMP Application.
install( AppDef ) ->
  Init = #{def=>AppDef, bufs=>[], mons=>[], procs=>[]},
  Funs = [
    {bufs,  fun install_buffers/1,    fun uninstall_buffers/2   },
    {procs, fun install_processors/1, fun uninstall_processors/2},
    {mons,  fun install_monitors/1,   fun uninstall_monitors/2  }
  ],
  NullRollback = fun(_RollbackReason) -> ok end,
  transaction(Funs, Init, NullRollback).

%% @doc Install the Application from LibEMP.
uninstall( Reason, #{bufs:=BufferRefs, procs:=ProcRefs, mons:=MonitorRefs} ) ->
  uninstall_monitors( Reason, MonitorRefs ),
  uninstall_processors( Reason, ProcRefs ),
  uninstall_buffers( Reason, BufferRefs ).

%%% =======================================================================
%%% Internal Functionality
%%% =======================================================================

%% @hidden
%% @doc The functionality for a transaction. Namely, on a failure we rollback
%%   all state we have accumulated. Otherwise we update our state and recurse
%%   until we reach the end of the function stream.
%% @end
transaction( [], State, _ ) -> {ok, State};
transaction( [{ID,Fun,UnFun}|Rest], State, Rollback ) ->
  case Fun( State ) of
    {error, Reason, Refs} ->
      RollbackReason = {shutdown,Reason},
      UnFun(RollbackReason, Refs),
      Rollback(RollbackReason),
      {error, Reason};
    {ok, Refs} ->
      NewState = State#{ID => Refs},
      NewRollback = fun(R) -> UnFun(R, Refs), Rollback(R) end,
      transaction(Rest, NewState, NewRollback)
  end.

%% @hidden
%% @doc Install the buffers the App defines.
install_buffers( #{def:=AppDef} ) ->
  InstallBuffer = fun( {Name,Module,Configs}, Refs ) ->
    case libemp_node:get_buffer( Name ) of
      {ok, _} -> get_or_error_buffer( Name, Refs );
      _ -> create_buffer( Name, Module, Configs, Refs )
    end
  end,
  libemp_app_def:foldl_buffers(InstallBuffer, [], AppDef).
get_or_error_buffer( default, Refs ) ->
  {ok, Initializer} = libemp_node:get_buffer(),
  [{default, ignore, Initializer} | Refs];
get_or_error_buffer( Name, Refs ) ->
  {error, {buffer_already_exists, Name}, Refs}.
create_buffer( Name, Module, Configs, Refs ) ->
  try
    {ok, Pid} = libemp_buffer_sup:add_buffer( Name, Module, Configs ),
    {ok, Initializer} = libemp_node:get_buffer( Name ),
    [ {Name,Pid,Initializer} | Refs ]
  catch _:Reason ->
    {error, Reason, Refs}
  end.

%% @hidden
%% @doc Uninstall the buffers that the App defines.
uninstall_buffers( _Reason, [] ) -> ok;
uninstall_buffers( Reason, [{default,_,_}|Others] ) ->
  uninstall_buffers( Reason, Others ); % Skip default buffer, don't shut it down
uninstall_buffers( Reason, [{_,Pid,Initializer}|Others] ) ->
  catch libemp_buffer_sup:remove_buffer( Reason, Pid, Initializer ),
  uninstall_buffers( Reason, Others ).

%% @hidden
%% @doc Install and link the Monitors to the Buffers they are attached to.
install_monitors( #{def:=AppDef} ) ->
  InstallMonitor = fun( {Name,Module,Configs,BufRef}, Refs ) ->
    case libemp_node:get_monitor( Name ) of
      {ok, _, _} -> {error, {monitor_already_exists, Name}, Refs};
      _ -> create_monitor( Name, Module, Configs, BufRef, Refs )
    end
  end,
  libemp_app_def:foldl_monitor( InstallMonitor, [], AppDef ).
create_monitor( Name, Module, Configs, BufRef, Refs ) ->
  try
    {ok, Pid} = libemp_monitor_sup:add_monitor(Name,Module,Configs,BufRef),
    [ Pid | Refs ]
  catch _:Reason ->
    {error, Reason, Refs}
  end.

%% @hidden
%% @doc For each monitor reference, shut it down with the given reason.
uninstall_monitors( _Reason, MonitorRefs ) ->
  lists:foreach( fun( Pid ) ->
                    catch libemp_monitor_sup:remove_monitor( Pid )
                 end,
                 MonitorRefs ).


%% @hidden
%% @doc Install and link the Processors to the Buffers they are attached to.
install_processors( #{def:=AppDef} ) ->
  InstallProcessor = fun( {BufRef, SinkModule, SinkConfigs, Handler}, Refs ) ->
    create_processor( BufRef, SinkModule, SinkConfigs, Handler, Refs )
  end,
  libemp_app_def:foldl_processors( InstallProcessor, [], AppDef ).
create_processor( BufRef, Module, Configs, _Handler, Refs ) ->
  try %%FIXME: if BufRef==default, we may need to inject the stack onto existing processors
    {ok, Pid} = libemp_processor_sup:add_processor( BufRef, Module, Configs ),
    [ Pid | Refs ]
  catch _:Reason ->
    {error, Reason, Refs}
  end.

%% @hidden
%% @doc For each Processor reference, shut it down with the given reason.
uninstall_processors( _Reason, ProcRefs ) ->
  lists:foreach( fun(Pid) ->
                    catch libemp_processor_sup:remove_processor( Pid )
                 end,
                 ProcRefs ).
