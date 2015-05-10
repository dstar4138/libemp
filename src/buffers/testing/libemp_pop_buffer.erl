%%% LibEMP Pop Buffer -
%%%
%%%     A buffer useful for testing Sinks. Will generate events at 
%%%     random intervals from a collection of known event types. The
%%%     `take' function will return a random number of events between
%%%     0 and the max (set by the system cfg, buffer_take_count).
%%%
-module(libemp_pop_buffer).
-behaviour(libemp_buffer).

%% Behaviour Exports
-export([initialize/1, register/2]).
%% Private Exports
-export([rand_events/1]).

initialize( Args ) -> init_handler( Args ).

register( _, State ) ->
    libemp_buffer:create([
        {take, fun() -> libemp_pop_buffer:rand_events(State) end},
        {give, fun(_) -> ok end},
        {size, fun() -> 0 end},
        {destroy, fun() -> ok end}
    ]).

%%% ======
%%% Private Functionality
%%% ======
-record(state, {possible_events, select_size, max}).

init_handler( Args ) ->
    {EventList,Size} = case proplists:lookup(buffer_event_list, Args ) of
        {buffer_event_list, []} -> {[event],1};
        {buffer_event_list, L} when is_list(L) -> {L, length(L)}
    end,
    Max = case proplists:lookup(buffer_take_count, Args) of
        {buffer_take_count, all} -> 1000;
        {buffer_take_count, N} when N > 0 -> N
    end,
    {ok, #state{possible_events=EventList,select_size=Size,max=Max}}.

rand_events(#state{possible_events=Set,select_size=S,max=N}) ->
    RandList = rand_list(S, random:uniform(N), []),
    lists:foldl(fun(Index,Acc)-> [lists:nth(Index,Set)|Acc] end, [], RandList).

rand_list( _, 0, L ) -> L;
rand_list( S, N, L ) -> rand_list( S, N-1, [random:uniform(S)|L] ).

