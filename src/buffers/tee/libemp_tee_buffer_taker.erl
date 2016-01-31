%%% LibEMP Tee Buffer Taker
%%%
%%%     Similar to the Simple buffer but on a per-Processor basis.
%%%
-module(libemp_tee_buffer_taker).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1,code_change/3,terminate/2,
  handle_call/3,handle_cast/2,handle_info/2]).

-record(taker,{stack=queue:new(),giver,component}).

%% @doc Start the Giver server.
start_link( {_Giver, _Component} = Args ) ->
  gen_server:start_link(?MODULE, Args, []).

%% @doc Initialize the state of the Taker Server.
init( {Giver, Component} ) ->
  {ok, #taker{giver=Giver,component=Component}}.

%% @doc Send the current queue to the taker when requested and make self
%%      available with empty queue.
%% @end
handle_call( take, _From, #taker{stack=S}=State ) ->
  {reply, queue:to_list(S), State#taker{stack=queue:new()}};
handle_call( unregister, _From, #taker{giver=Giver}=State ) ->
  gen_server:call( Giver, {unregister, self()}),
  {stop, normal, State};
handle_call( _Ignore, _From, State ) ->
  {reply, {error, badarg}, State}.

%% @doc Handle (broad/multi/single)-casts. In this case, just the `give'
%%   operations.
%% @end
handle_cast( {give,E}, #taker{stack=Q}=State ) ->
  {noreply, State#taker{stack=queue:in(E,Q)}};
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
