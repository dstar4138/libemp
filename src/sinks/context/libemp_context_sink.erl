%%% LibEMP Context Sink -
%%%
%%%  The Context sink works as a context provider to later Sinks by adding how
%%%  many of a particular Event the System has seen.
%%%
%%%  To configure an instance of this Sink, it needs the Typer function to
%%%  capture what type of an event it was. We define the concept of a
%%%  ContextualEvent to be the map: #{ context => #{...}, event => ... }.
%%%
-module(libemp_context_sink).
-behaviour(libemp_sink).
-include("libemp.hrl"). % Types

%% Sink API
-export([setup/1]).
-export([process/3]).

%% Misc APIs
-export([add_contexter/2]).

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

-record(context, {
          typer=?DEFAULT_EVENT_TYPE_FUN, % The function used to find event type.
          counters = #{},                % Map of Type to CONTEXT_COUNTER.
          contexters = []                % User provided functions for context.
        }). 
-type state() :: #context{}.

%% @doc Set up the default state based on the selected mode.
-spec setup( [ {atom(), term()} ] ) -> {ok, state()} | {stop, atom()}.
setup( Args ) -> build_context_state( Args ).

%% @doc Process an event based on the mode the sink is in.
-spec process( libemp_event(), term(), state() ) -> 
    {next, state()} | {next, libemp_event(), state()} | {drop, state()}.
process( Event, _, State ) -> context(Event, State).

%%% ==========================================================================
%%% Misc API
%%% ==========================================================================

%% @doc Add a contextual function to the Context Sink.
add_contexter( Function, State = #context{contexters = Cs} )
  when is_function( Function, 2 ) ->
    DefaultContexterState = [],
    NewItem = {Function, DefaultContexterState},
    {ok, State#context{contexters = [NewItem|Cs]}};
add_contexter( _, _ ) ->
  {error, badarg}.


%%% ==========================================================================
%%% Internal Functionality
%%% ==========================================================================

%% @hidden
%% @doc Append context to a contextual event, or create a contextual event from
%%   the provided Event and Context.
%% @end
append_context( #{context := PrevContext} = ContextualEvent, Context ) ->
    ContextualEvent#{context => maps:merge(PrevContext,Context)};
append_context( Event, Context ) ->
    #{context => Context, event => Event}.

%% @hidden
%% @doc Setup the state for the contextualizing counter sink.
build_context_state( _Args ) ->
    %TODO: Get the context's arguments, for now return default contexter.
    %   i.e turn on and off the default contextualizers and get user provided.
    {ok, #context{}}. 

%% @hidden
%% @doc Add more context to the event by wrapping the event in a context map.
context( Event, State=#context{typer=TyperFun} ) ->
    Type = TyperFun( Event ),
    {Context, NewState} = build_context_for_event( Type, State ),
    ContextualEvent = append_context( Event, Context ),
    {next, ContextualEvent, NewState}.

%% @hidden
%% @doc For all the context functions, we pass the type of the event in and
%%   generate a context to add to it.
%% @end
build_context_for_event( Type, CurState ) ->
  lists:foldl(fun(Contextualizer, {Context,State}) ->
    {MoreContext, NewState} = Contextualizer(Type, State),
    {maps:merge(MoreContext, Context), NewState}
    end, {maps:new(), CurState}, contextualizers()).

%%% ==========================================================================
%%% Contextualizers
%%% ==========================================================================

%% @hidden
%% @doc Return the list of functions which will add context to Events which
%%    meet a particular type.
%% @end
contextualizers() -> [
  fun c_context_type/2,
  fun c_serialized_count/2,
  fun c_timestamp/2,

  %% User provided should always go last, to provide override ability.
  fun c_user_provided/2
].

%% @hidden
%% @doc The type the event matched.
c_context_type(Type,State) ->
  Context = #{type => Type},
  {Context, State}.

%% @hidden
%% @doc The current count seen of this type.
c_serialized_count(Type, State=#context{counters=Map}) ->
  CurCount = maps:get( Type, Map, 0 ) + 1,
  NewMap = Map#{ Type => CurCount },
  Context = #{count => CurCount},
  NewState = State#context{counters=NewMap},
  {Context, NewState}.

%% @hidden
%% @doc Get the current timestamp in a non-blocking fashion.
c_timestamp( _,  State ) ->
  Context = #{localts => erlang:system_time()},
  {Context, State}.

%% @hidden
%% @doc Loop over the user provided contextualizers and allow them to generate
%%    context for the provided event type.
%% @end
c_user_provided( Type, #context{contexters=Funs}) ->
  lists:foldl(fun({Contexter,State}, {Context, PrevFuns}) ->
    {MoreContext,NewState} = Contexter(Type,State),
    {maps:merge(MoreContext,Context), [{Contexter,NewState}|PrevFuns]}
    end, {maps:new(), []}, Funs).

