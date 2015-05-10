%%% LibEMP Multi-Queue Buffer -
%%%
%%%     This LibEMP Event Buffer implements a unique queue per Monitor 
%%%     component. When a Sink wishes to Take from the Buffer, it round-robin
%%%     selects a queue and trades it with an empty one. This has the effect of
%%%     a non-constant time Take, and non-serialized event processing but earns
%%%     minimal read contention and batched event processing.
%%%
%%%     Statistics:
%%%
%%%
-module(libemp_multiq_buffer).

%% Buffer Callback Initialization
-behaviour(libemp_buffer).
-export([initialize/1, register/2]).

%% Buffer Implementation
-behaviour(gen_server).
-export([init/1,code_change/3,terminate/2,
         handle_call/3,handle_cast/2,handle_info/2]).

%% @doc Initialize the multi-queue buffer.
initialize( Args ) ->
    gen_server:start_link( {local, ?MODULE}, ?MODULE, Args, [] ).

register( TakerGiver, Pid ) ->
    {ok, NewPid} = gen_server:call( Pid, {register, TakerGiver} ),
    libemp_buffer:create([
        {take, fun() -> gen_server:call( NewPid, take ) end},
        {give, fun(E) -> gen_server:cast( NewPid, {give, E} ) end},
        {size, fun() -> gen_server:call( Pid, size) end},
        {destroy, fun() -> exit( Pid, shutdown ) end}
    ]).

%%% ==========================================================================
%%% Taker Server
%%% ==========================================================================
-record(taker,{givers=queue:new()}).
-record(giver,{stack=queue:new(),taker,component}).

%% @doc Initialize the Take Handler.  
init( {giver,Taker,Component} ) -> 
    {ok, #giver{taker=Taker,component=Component}};
init( _Args ) -> 
    {ok, #taker{}}. 

handle_call( take, _From, #taker{givers=Givers}=State ) ->
    {ok, Next, NewGivers} = round_robin( Givers ),
    case Next of 
        empty -> {reply, [], State};
        Giver -> 
            Result = gen_server:call( Giver, take ),
            {reply, Result, State#taker{givers=NewGivers}}
    end;
handle_call( take, _From, #giver{stack=S}=State ) ->
    Reply = {ok, S},
    {reply, Reply, State#giver{stack=queue:new()}}; 

handle_call( {register, TakeOrGive}, From, #taker{givers=Gs}=State ) ->
    case TakeOrGive of 
        take -> {reply, {ok, self()}, State}; % Primary is Taker
        give -> % Spawn new Giver for individual
            {ok, Giver} = gen_server:start_link( {local,{giver,queue:len(Gs)}}, 
                                                 ?MODULE, 
                                                 {giver, self(), From},
                                                 [] ),
            {reply, {ok, Giver}, State#taker{givers=queue:in(Giver,Gs)}}
    end;
handle_call( size, _From, #taker{givers=Givers}=State ) ->
    {reply, queue:len(Givers), State}; % Assume size is length of Giver pool.
handle_call( _Ignore, _From, State ) -> 
    {reply, {error, badarg}, State}.

handle_cast( {give,E}, #giver{stack=Q}=State ) -> 
    {noreply, State#giver{stack=queue:in(E,Q)}};
handle_cast( _Ingore, State ) -> 
    {noreply, State}.

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

%% @hidden
%% @doc Select the next giver from the queue and reinsert it.
round_robin( Givers ) ->
    case queue:out(Givers) of
        {{value, Next}, Rest} -> {Next, queue:in(Next, Rest)};
        {empty, Empty} -> {empty, Empty}
    end.

