%%% LibEMP Shape Sink -
%%%
%%%  The Shape Sink acts as a traffic shaping Sink to reduce the number of
%%%  events needed to process later in the stack. It uses a typer function
%%%  given to it in order to classify the event. Once classified it will
%%%  keep track of the number of times it has seen that type within a window.
%%%  If the number of times reaches a threshold, it will let the newest
%%%  event through.
%%%
-module(libemp_shape_sink).
-behaviour(libemp_sink).
-include("libemp.hrl"). % Types

%% Sink API
-export([setup/1]).
-export([process/3]).

%%% By default, the type of event is recognized by the first tuple value,
%%% otherwise it is the whole event.
-define(DEFAULT_EVENT_TYPE_FUN, 
        fun( Event ) ->
            % Potentially unwrap context map.
            Unwrap = case Event of #{event := E} -> E; _ -> Event end, 
            case is_tuple(Unwrap) of 
                true -> element(1, Unwrap);
                false -> Unwrap 
            end
        end).

%%% ==========================================================================
%%% LibEMP Sink API
%%% ==========================================================================

-record(shape, {
          typer=?DEFAULT_EVENT_TYPE_FUN, % The function used to find event type
          threshold=5,                   % The count threshold for triggering
          window_time=5,                 % Timeframe in seconds for smoothing
          counters = #{}                 % Map of Type to SHAPE_COUNTER.
        }).

%% The shape_counter is a description of the current time window for each 
%% event type (as the windows can be shifted differently depending on when the
%% type started showing up).
%% The layout is as follows:
%%      { 
%%       HAS_TRIGGERED_THIS_WINDOW,
%%       PREVIOUS_WINDOW_BORDER_TIME,
%%       CURRENT_WINDOW_COUNT 
%%      }

-define(DEFAULT_SHAPE_COUNTER, {false, get_current_time(), 0}).
-type state() :: #shape{}.

%% @doc Set up the default state based on the selected mode.
-spec setup( [ {atom(), term()} ] ) -> {ok, state()}.
setup( Args ) -> build_shape_state( Args ).

%% @doc Process an event based on the mode the sink is in.
-spec process( libemp_event(), term(), state() ) -> 
    {next, state()} | drop | {drop, state()}.
process( Event, _, State ) -> shape(Event, State).

%%% ==========================================================================
%%% Internal Functionality
%%% ==========================================================================

%% @hidden
%% @doc Setup the state for the traffic shaping counter sink.
build_shape_state( _Args ) ->
    %TODO: Get the shape's arguments, for now return default shaper.
    {ok, #shape{}}.

%% @hidden
%% @doc Get the current time in seconds. We use a monotonic timestamp which may
%%   work well with time warps if multi_time_warp is turned on, otherwise this
%%   may hang longer than expected with no_time_warp turned on.
%% @end
get_current_time() -> erlang:abs( erlang:monotonic_time(seconds) ).

%% @hidden
%% @doc Shape the oncoming traffic by counting "like" events until a threshold
%%   and then allow the newest event through.
%% @end
shape( Event, State=#shape{typer=TyperFun, counters=Map} ) ->
    Type = TyperFun( Event ),
    CurTime = get_current_time(),
    {Triggered, WindowBorder, CurCount} = 
            maps:get( Type, Map, ?DEFAULT_SHAPE_COUNTER ),
    case
        { 
            State#shape.window_time < (CurTime - WindowBorder),
            Triggered, 
            State#shape.threshold < CurCount
        } 
    of
        % Window is over, reset the window and add one to count. 
        {true, _, _} ->
            NewValue = {false, CurTime, 1},
            NewMap = Map#{ Type => NewValue },
            {drop, State#shape{ counters=NewMap }};

        % Drop the message outright if we've already triggered this window.
        {_, true, _} -> drop;

        % We have not triggered yet, and we have to, then allow it to pass
        % and update the state of the counter.
        {_, false, true} ->
            NewValue = {true, WindowBorder, CurCount+1},
            NewMap = Map#{ Type => NewValue },
            {next, State#shape{ counters=NewMap }};

        %% We have not triggered yet, but we don't have to, drop it.
        {_, false, false} ->
            NewValue = {false, WindowBorder, CurCount+1},
            NewMap = Map#{ Type => NewValue },
            {drop, State#shape{ counters=NewMap }}
    end.

