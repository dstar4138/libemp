%%% LibEMP Multi-Queue Buffer -
%%%
%%%     This LibEMP Event Buffer implements a unique queue per Monitor 
%%%     component. When a Sink wishes to Take from the Buffer, it round-robin
%%%     selects a queue and trades it with an empty one. This has the effect of
%%%     a non-constant time Take, and non-serialized event processing but earns
%%%     minimal read contention and batched event processing reaching the 
%%%     theoretical limit of erlang's gen_server message handling.
%%%
%%%     Statistics:
%%%
%%%
-module(libemp_multiq_buffer).

%% Buffer Callback Initialization
-behaviour(libemp_buffer).
-export([initialize/1, register/2, destroy/1]).

%% @doc Initialize the multi-queue buffer. See the master supervisor for 
%%   details on startup mechanism.
%% 
%% @see libemp_multiq_buffer_sup
%% @end
initialize( Args ) ->
    {ok, Pid} = libemp_multiq_buffer_sup:start_link( Args ),
    {ok, Pid, {Pid, ?MODULE}}.

%% @doc Register a taker/giver by contacting the registration system created via
%%   `initialize/0'. For `gen_server' call implementations see taker and giver 
%%   modules.
%%
%% @see libemp_multiq_buffer_taker
%% @see libemp_multiq_buffer_giver
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

