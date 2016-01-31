%%% LibEMP Epocxy Buffer - 
%%%
%%%     An Event Buffer using the Epocxy library's ets_buffer module. We can 
%%%     test multiple buffer models such as fifo/lifo/ring and with different
%%%     batched reading mechanisms (all, N>1, etc). Additionally this serves as
%%%     a decent example as to how to wrap a generic queue implementation as 
%%%     a LibEMP Buffer. 
%%%
%%%     Note: It looks like the read mechanism does not guarantee uniqueness,
%%%     and it may be the case that successive takes contain the same event(s).
%%%
-module(libemp_epocxy_buffer).
-behaviour(libemp_buffer).

% libemp_buffer Behaviour Callbacks. 
-export([initialize/1,
    validate_configs/1,
    register/2,destroy/1]).

%% By default it's a FIFO queue of "unlimited" size.
%% You can modify this with the following Args via libemp_buffer:start/2;
%% {buffer_type, Type} - The type of input queue this is, can be fifo/lifo/ring.
%% {buffer_size, pos_integer()} - Size of the ring buffer. Unused if fifo/lifo.
%% {buffer_take_count, all | pos_integer()} - # items to return on a take/1.
-define(DEFAULT_CONFIGS, {fifo, 0, all}).

%% @doc Initialize the ets buffer and wrap the module calls in LibEMP Buffer
%%   API calls.
%% @end
initialize( Args ) ->
    {ok, Reference, ReadCount} = build_ets_buffer_args_dedicated( Args ),
    {ok, {Reference,ReadCount}}.

%% @doc Wrap the module calls in LibEMP Buffer API Calls.
register( _TakerGiver, {Reference,ReadCount} ) ->
    Take = prime_read_dedicated(Reference, ReadCount),
    Give = fun(Event) -> ets_buffer:write_dedicated(Reference,Event) end,
    Size = fun() -> ets_buffer:num_entries_dedicated(Reference) end, 
    Unregister = fun() -> ok end,
    libemp_buffer:create([
            {take, Take},
            {give, Give},
            {size, Size},
            {unregister, Unregister}
    ]).

%% @doc Destroy the whole ets buffer.
destroy( {Reference, _} ) ->
    ets_buffer:delete_dedicated( Reference ).

%% @doc Let LibEMP check the provided configs, to allow LibEMP to fail early.
validate_configs( Args ) ->
    case merge_args( Args, ?DEFAULT_CONFIGS ) of
        {_,_,N} when is_integer(N) andalso N < 1 ->
                        {error, "Take count is less than 1."};
        {ring,0,_}   -> {error, "Ring is missing size."};
        {lifo,_,all} -> {error, "Read-All does not work with lifo."};
        {Type,_,_} when Type =/= lifo andalso
                        Type =/= fifo andalso
                        Type =/= ring ->
            {error, "Unknown ets buffer type; must be: ring, fifo, or lifo."};
        _Otherwise ->
            ok
    end.

%%% =========================================================================
%%% Private Functionality
%%% =========================================================================

%% @hidden
%% @doc Determine the buffer arguments from our defaults and the passed in 
%%   arguments.
%% @end
merge_args([], Configs) -> Configs;
merge_args([{buffer_take_count,Count}|Rest], {T,S,_}) -> merge_args(Rest,{T,S,Count});
merge_args([{buffer_type,Type}|Rest], {_,S,C}) -> merge_args(Rest,{Type,S,C});
merge_args([{buffer_size,Size}|Rest], {T,_,C}) -> merge_args(Rest,{T,Size,C});
merge_args([_UnknownArgument|Rest], Configs) -> merge_args(Rest, Configs).

%% @hidden
%% @doc Build the `epocxy' ets_buffer for use as the LibEMP Event Buffer.
build_ets_buffer_args_dedicated(Args) ->
    case merge_args( Args, ?DEFAULT_CONFIGS ) of
        {ring,S,R} when S > 0 ->
            Tid = ets_buffer:create_dedicated(libemp_ets_event_buffer,ring,S),
            {ok, Tid, R};
        {Type,_,R} when Type =:= fifo orelse Type =:= lifo ->
            Tid = ets_buffer:create_dedicated(libemp_ets_event_buffer,Type),
            {ok, Tid, R}
    end.

%% @hidden
%% @doc Build the Take api call based on the type of read call has been asked
%%   for in the configuration.
%% @end
prime_read_dedicated(Ref,all) -> 
    fun() -> ets_buffer:read_all_dedicated(Ref) end;
prime_read_dedicated(Ref,N) when N > 0 -> 
    fun() -> ets_buffer:read_dedicated( Ref, N ) end.

