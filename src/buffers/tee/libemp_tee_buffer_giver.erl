%%% LibEMP Tee Buffer Giver
%%%
%%%     Tee is a buffer implementation which has a single server which
%%%     handles all `Give' operations, but spawns a new server for each
%%%     registered process which will be performing a `take'. The Giver Server
%%%     keeps track of these Taker Servers to broadcast to.
%%%
-module(libemp_tee_buffer_giver).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1,code_change/3,terminate/2,
  handle_call/3,handle_cast/2,handle_info/2]).

-record(giver,{takers=[]}).
-define(REGISTRATION_NAME, libemp_tee_buffer).

%% @doc Start the Giver/Registration server.
start_link( Args ) ->
  gen_server:start_link({local, ?REGISTRATION_NAME}, ?MODULE, Args, []).

%% @doc Initialize the Giver Handler.
init( _Args ) ->
  {ok, #giver{}}. %TODO: what args are needed here?

%% @doc Handle the synchronous API calls: `register' and `size'.
handle_call( {register, TakeOrGive}, From, State ) ->
  case TakeOrGive of
    give -> {reply, {ok, self()}, State}; % Primary is Giver
    take -> % Spawn new Taker for individual
      {ok, Taker} = libemp_tee_buffer_ts:new_child( {self(), From} ),
      {reply, {ok, Taker}, add_taker( Taker, State )}
  end;
handle_call( size, _From, #giver{takers=Takers}=State ) ->
  {reply, queue:len(Takers), State}; % Assume size is length of Taker pool.
handle_call( unregister, _From, State ) ->
  {reply, ok, State}; % Don't need to do anything for giver.
handle_call( _Ignore, _From, State ) ->
  {reply, {error, badarg}, State}.

%% @doc Handle (broad/multi/single)-casts.
handle_cast( {give, Event}, #giver{takers=Takers}=State ) ->
  broadcast( Event, Takers ),
  {noreply, State};
handle_cast( {unregister, GiverPid}, State ) ->
  {noreply, rm_taker(GiverPid, State)};
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
broadcast( Event, Takers ) ->
  lists:foldl( fun(T,_) -> gen_server:cast(T,{give,Event}) end, [], Takers ).

%% @hidden
%% @doc Add a new Queue reference to the list of queues.
add_taker( Taker, #giver{takers=Ts}=State ) ->
  State#giver{takers=[Taker|Ts]}.

%% @hidden
%% @doc Remove a Queue reference from the list of queues.
rm_taker( Taker, #giver{takers=Ts}=State ) ->
  NewTs = lists:filter( fun( T ) -> T /= Taker end, Ts),
  State#giver{takers=NewTs}.