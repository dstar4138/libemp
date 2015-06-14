%%% LibEMP MultiQ Buffer Taker
%%%
%%%     MultiQ is a buffer implementation which has a single server which
%%%     handles all `take' operations, but spawns a new server for each 
%%%     registered process which will be performing a `give'. The Taker Server
%%%     keeps track of these Giver Servers to round-robin 
%%%
-module(libemp_multiq_buffer_taker).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1,code_change/3,terminate/2,
         handle_call/3,handle_cast/2,handle_info/2]).

-record(taker,{givers=queue:new()}).
-define(REGISTRATION_NAME, libemp_multiq_buffer).

%% @doc Start the Taker/Registration server.
start_link( Args ) ->
    gen_server:start_link({local, ?REGISTRATION_NAME}, ?MODULE, Args, []).

%% @doc Initialize the Take Handler.  
init( _Args ) -> 
    {ok, #taker{}}. %TODO: what args are needed here? 

%% @doc Handle the synchronous API calls: `take', `register' and `size'.
handle_call( take, _From, #taker{givers=Givers}=State ) ->
    {Next, NewGivers} = round_robin( Givers ),
    case Next of 
        empty -> {reply, [], State};
        Giver -> 
            Result = gen_server:call( Giver, take ),
            {reply, Result, State#taker{givers=NewGivers}}
    end;
handle_call( {register, TakeOrGive}, From, State ) ->
    case TakeOrGive of 
        take -> {reply, {ok, self()}, State}; % Primary is Taker
        give -> % Spawn new Giver for individual
            {ok, Giver} = libemp_multiq_buffer_gs:new_child( {self(), From} ),
            {reply, {ok, Giver}, add_giver( Giver, State )} 
    end;
handle_call( size, _From, #taker{givers=Givers}=State ) ->
    {reply, queue:len(Givers), State}; % Assume size is length of Giver pool.
handle_call( unregister, _From, State ) -> 
    {reply, ok, State}; % Don't need to do anything for takers.
handle_call( _Ignore, _From, State ) -> 
    {reply, {error, badarg}, State}.

%% @doc Handle (broad/multi/single)-casts.
handle_cast( {unregister, GiverPid}, State ) ->
    {noreply, rm_giver(GiverPid, State)};
handle_cast( _Ingore, State ) -> {noreply, State}.


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
    case queue:out( Givers ) of
        {{value, Next}, Rest} -> {Next, queue:in(Next, Rest)};
        {empty, Empty} -> {empty, Empty}
    end.

%% @hidden
%% @doc Add a new Queue reference to the list of queues.
add_giver( Giver, #taker{givers=Gs}=State ) ->
    State#taker{givers=queue:in(Giver,Gs)}.

%% @hidden
%% @doc Remove a Queue reference from the list of queues.
rm_giver( Giver, #taker{givers=Gs}=State ) ->
    NewGs = lists:filter( fun( G ) -> G /= Giver end, queue:to_list( Gs ) ),
    State#taker{givers=queue:from_list(NewGs)}.

