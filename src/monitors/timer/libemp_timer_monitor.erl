%%% Timer Plug-in -
%%%
-module(libemp_timer_monitor).
-behaviour(libemp_monitor).
-vsn({1,0,0}).

%% LibEMP Monitor API
-export([ describe/2, setup/3, destroy/2 ]).

%% Private exports
-export([loop/2]).

%% LibEMP Monitor Definition.
-define(MONITOR_CONFIG, []).

%% Our event structure that we generate.
-define(TICK_EVENT(Minutes,Hours,Day,Month,DayOfWeek),
  {tick,Minutes,Hours,Day,Month,DayOfWeek}).
-define(FRACTION, 10*1000).

%%% ==========================================================================
%%% LibEMP Monitor Callbacks
%%% ==========================================================================

%% @doc Return default name and libemp configuration for the Cron monitor. We
%%   do not need to check the host to see if we are able to run as there are no
%%   requirements.
%% @end
describe( _Name, _Args ) ->
  {ok, ?MONITOR_CONFIG}.

%% @doc Initialize our Monitor.
setup( _Args, _Config, EMP ) ->
  Pid = spawn(?MODULE,loop,[EMP, current_tick()]),
  {ok,Pid}.

%% @doc Shutdown our Monitor by killing the timer.
destroy( _Reason, Pid ) ->
  Pid ! shutdown.

%%% ==========================================================================
%%% Private Functionality
%%% ==========================================================================

%% @private
%% @doc Our looping function, we check the local time, and if a minute
%%   has passed on the wall-clock, trigger an event.
%% @end
loop(EMP, PreviousTick) ->
  receive
    shutdown ->
      ok;
    _ -> % Ignore a message but force a trigger check.
      check_if_trigger(EMP,PreviousTick)
  after % After every fraction of a minute check if the tick time changed.
    ?FRACTION ->
      check_if_trigger(EMP,PreviousTick)
  end.

%% @hidden
%% @doc Check if the event should trigger, then jump back into `loop/2'.
check_if_trigger(EMP,PreviousTick) ->
  CurrentTick = current_tick(),
  case CurrentTick of
    PreviousTick -> ok;
    NewTick -> libemp_monitor_api:emit(NewTick, EMP)
  end,
  erlang:apply(?MODULE, loop, [EMP, CurrentTick]).

%% @hidden
%% @doc Get the current "tick" event based on the system time.
current_tick() ->
  {{Year,Month,Day}, {Hour,Minute,_sec}} = calendar:local_time(),
  WkDay = case calendar:day_of_the_week(Year,Month,Day) of
            7 -> 0; % We want Sunday = 0 not 7.
            N -> N
          end,
  ?TICK_EVENT(Minute,Hour,Day,Month,WkDay).