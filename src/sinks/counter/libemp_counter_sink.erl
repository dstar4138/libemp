%%% LibEMP Counter Sink -
%%%
%%%  The Counter sink performs two functions, it can act as a traffic shaping
%%%  Sink to reduce the number of events needed to process later in the stack;
%%%  it can also work as a context provider to later Sinks by adding how many
%%%  of a particular Event the System has seen.
%%%
%%%  To configure an instance of this Sink, it needs at least two things, the
%%%  mode it is in (shape vs context) and the Typer function to capture what
%%%  type of an event it was. This Sink also may modify the event insofar as to
%%%  provide more context to the event. We therefore define the concept of a 
%%%  ContextualEvent to be the map: #{ context => #{...}, event => ... }.
%%%
-module(libemp_counter_sink).
-behaviour(libemp_sink).
-include("../../internal.hrl").

%% Sink API
-export([setup/1]).
-export([process/3]).

%% Misc APIs
-export([list_modes/0]).
-export([add_context/2]).

%% The potential modes that can be passed into setup.
-define(POSSIBLE_MODES, [shape, context]).

%% By default, the Counter sink seeks to shape traffic for 
-define(DEFAULT_MODE, shape).

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
%%       HAS_TRIGGERED_THIS WINDOW,
%%       PREVIOUS_WINDOW_BORDER_TIME,
%%       CURRENT_WINDOW_COUNT 
%%      }
-define(DEFAULT_SHAPE_COUNTER, {false, get_current_time(), 0}).
-record(context, {
          typer=?DEFAULT_EVENT_TYPE_FUN, % The function used to find event type.
          counters = #{}                 % Map of Type to CONTEXT_COUNTER.
        }). 
-type state() :: #shape{} | #context{}.

%% @doc Set up the default state based on the selected mode.
-spec setup( [ {atom(), term()} ] ) -> {ok, state()} | {stop, atom()}.
setup( Args ) ->
    case proplists:get_value( mode, Args, ?DEFAULT_MODE ) of
        shape -> build_shape_state( Args );
        context -> build_context_state( Args )
    end.

%% @doc Process an event based on the mode the sink is in.
-spec process( libemp_event(), term(), state() ) -> 
    {next, state()} | {next, libemp_event(), state()} | {drop, state()}.
process( Event, _, #shape{}=State ) -> shape(Event, State);
process( Event, _, #context{}=State ) -> context(Event, State).

%%% ==========================================================================
%%% Misc API
%%% ==========================================================================

%% @doc Get a list of the possible modes this Sink can be initialized for. The
%%   chosen mode can then be passed into the libemp_sink:start/1 function to 
%%   get the sink.
%% @end
-spec list_modes() -> [atom()].
list_modes() -> ?POSSIBLE_MODES.

%% @doc Append context to a contextual event, or create a contextual event from
%%   the provided Event and Context.
%% @end
add_context( #{context := PrevContext} = ContextualEvent, Context ) ->
    ContextualEvent#{context => maps:merge(PrevContext,Context)};
add_context( Event, Context ) ->
    #{context => Context, event => Event}.

%%% ==========================================================================
%%% Internal Functionality
%%% ==========================================================================

%% @hidden
%% @doc Setup the state for the traffic shaping counter sink.
build_shape_state( _Args ) ->
    %TODO: Get the shape's arguments, for now return default shaper.
    {ok, #shape{}}.

%% @hidden
%% @doc Setup the state for the contextualizing counter sink.
build_context_state( _Args ) ->
    %TODO: Get the context's arguments, for now return default contexter.
    {ok, #context{}}. 

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
         
%% @hidden
%% @doc Add more context to the event by wrapping the event in a context map.
context( Event, State=#context{typer=TyperFun, counters=Map} ) ->
    Type = TyperFun( Event ),
    CurCount = maps:get( Type, Map, 0 ) + 1,
    NewMap = Map#{ Type => CurCount },
    Context = #{count => CurCount}, 
    ContextualEvent = add_context( Event, Context ),
    {next, ContextualEvent, State#context{counters=NewMap}}.

