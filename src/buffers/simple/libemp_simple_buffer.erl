%%% Simple Queue Event Buffer -
%%%
%%%     An in memory queue using the Erlang IPC Mailbox system to serialize
%%%     and gen_server state for storage. This is used for testing and 
%%%     comparison to other buffer mechanisms.
%%%
%%%     Statistics:
%%%         * Single Perfect Producer: > 700k eps
%%%         * Single Perfect Consumer: > 350k eps
%%%         * Throughput: take1(> 250k eps), takeAll(> 300k eps)
%%%
-module(libemp_simple_buffer).

%% Buffer Callback Initialization 
-behaviour(libemp_buffer).
-export([initialize/1,register/2,destroy/1]).

%% Buffer Implementation
-behaviour(gen_server).
-export([init/1,code_change/3,terminate/2,
         handle_call/3,handle_cast/2,handle_info/2]).

%% @doc Initialize the simple, in-memory buffer. This wraps the `gen_server'
%%   API calls into the `libemp_buffer' object for use in the monitoring 
%%   platform.
%% @end
initialize( Args ) ->
    {ok, Pid} = gen_server:start_link( {local, ?MODULE}, ?MODULE, Args, [] ),
    {ok, Pid, ?MODULE}.

%% @doc Register either a taker or giver with the buffer PID, by just returning
%%   the gen_server API for it.
%% @end
register( _TakerGiver, Ref ) ->
    libemp_buffer:create( [
                    {take, fun()  -> gen_server:call( Ref, take ) end},
                    {give, fun(E) -> gen_server:cast( Ref, {give,E} ) end},
                    {size, fun()  -> gen_server:call( Ref, size ) end},
                    {unregister, fun() -> ok end} % ignore
    ]).

%% @doc Ask the process tree to shutdown.
destroy( Ref ) ->
    case whereis(Ref) of
        Pid when is_pid(Pid) ->
            exit( Pid, normal ),
            hang_for_destroy( Ref ); % TODO: really hang here?
        _ -> 
            ok % Consider destroyed.
    end.

%% @doc Initialize the simple queue server.
init( Args ) -> 
    process_flag(trap_exit, true), 
    init_handler( Args ).

%% @doc Handle gen_server calls. Currently used only for event removals.
handle_call( take, _From, State ) -> take_handler( State );
handle_call( size, _From, State ) -> size_handler( State );
handle_call( _Ignore, _From, State ) -> {reply, {error,badarg}, State}.

%% @doc Handle gen_server casts. Currently used only for event additions.
handle_cast( {give, Event}, State ) -> give_handler( Event, State );
handle_cast( _Ignore, State ) -> {noreply, State}.

%%% ======
%%% Default gen_server callbacks
%%% ======

%% @doc Handle any system messages.
handle_info( _Message, State ) -> {noreply, State}.

%% @doc Error state on any code change as unexpected.
code_change( _OldVsn, _State, Extra ) -> {error, {unexpected, Extra}}.

%% @doc Terminate the buffer service and buffer.
terminate( _Reason, _State ) -> ok.

%%% ======
%%% Private functionality
%%% ======
-record(state, {queue = queue:new(), take_count = 1}).

init_handler( Args ) ->
    case proplists:lookup(buffer_take_count, Args) of
        {buffer_take_count, all} -> {ok, #state{take_count=all}};
        {buffer_take_count, N} when N >= 1 -> {ok, #state{take_count=N}};
        none -> {ok, #state{}};
        _    -> {stop, {badarg, buffer_take_count}}
    end.
take_handler( #state{queue=Q, take_count=C}=S ) ->
    {V, NQ} = out(Q,C), 
    {reply, V, S#state{queue=NQ}}.
size_handler( #state{queue=Q}=S ) -> {reply, queue:len(Q), S}.
give_handler( V, #state{queue=Q}=S ) -> {noreply, S#state{queue=queue:in(V,Q)}}.

out(Q,all)->
    {queue:to_list(Q),queue:new()};
out(Q,1) ->
    case queue:out(Q) of
        {{value,V},NQ} -> {[V],NQ};
        {empty,Q}      -> {[],Q}
    end;
out(Q,N) ->
    {Top,Rest} = lists:split(N, queue:to_list(Q)),
    {Top, queue:from_list(Rest)}.

hang_for_destroy( Ref ) ->
    case whereis( Ref ) of
        Pid when is_pid(Pid) -> hang_for_destroy( Ref );
        _ -> ok
    end.
