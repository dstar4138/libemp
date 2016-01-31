%%% LibEMP Tee Buffer -
%%%
%%%     This LibEMP Event Buffer implements a unique queue per Processor
%%%     component. When a Sink wishes to Take from the Buffer, it takes from its
%%%     own queue. On the Monitor side, a Give operation must append to all
%%%     registered takers. This has the effect of a non-constant time Give,
%%%     but earns minimal read contention, serialized and consistent event
%%%     streams, and batched event processing. This is the spiritual sibling
%%%     to the MultiQ Buffer.
%%%
%%%     Statistics:
%%%         * Single Perfect Producer: > 700k eps
%%%         * Single Perfect Consumer: > 350k eps
%%%         * Throughput: takeAll(> 400k eps)
%%%
-module(libemp_tee_buffer).

%% Buffer Callback Initialization
-behaviour(libemp_buffer).
-export([initialize/1, register/2, destroy/1]).

%% @doc Initialize the Tee buffer. See the master supervisor for
%%   details on startup mechanism.
%%
%% @see libemp_tee_buffer_sup
%% @end
initialize( Args ) ->
  {ok, Pid} = libemp_tee_buffer_sup:start_link( Args ),
  {ok, Pid, {Pid, ?MODULE}}.

%% @doc Register a taker/giver by contacting the registration system created via
%%   `initialize/0'. For `gen_server' call implementations see taker and giver
%%   modules.
%%
%% @see libemp_tee_buffer_taker
%% @see libemp_tee_buffer_giver
%% @end
register( TakerGiver, {_, Ref} ) ->
  {ok, NewPid} = gen_server:call( Ref, {register, TakerGiver} ),
  libemp_buffer:create([
    {take, fun() -> gen_server:call( NewPid, take ) end},
    {give, fun(E) -> gen_server:cast( NewPid, {give, E} ) end},
    {size, fun() -> gen_server:call( Ref, size) end},
    {unregister, fun() -> gen_server:call( NewPid, unregister ) end}
  ]).

%% @doc Call the master supervisor to shut down the OTP structure.
destroy( {Top, _} ) ->
  exit( Top, normal ).