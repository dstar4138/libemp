%%% Buffer testing
-module(buffer_throughput).
-export([run_drop/0,
         run_simple/0,
         run_epocxy/0,
         run_batchets/0]).

-define(FILLUP_COUNT, 1000000).

run_tests( Buffer ) ->
    io:format("Running ~p fill up test with ~p producers...~n",[?FILLUP_COUNT, 1]),
    time_run(fun fillup_test/2, [Buffer,1]),
%   io:format("SIZE: ~p~n",[ libemp_buffer:size( Buffer )]),
    
    io:format("=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-~n~n"),
    io:format("Running ~p pull out test with ~p consumers...~n",[?FILLUP_COUNT, 1]),
    time_run(fun pullout_test/2, [Buffer,1]),
%   io:format("SIZE: ~p~n",[ libemp_buffer:size( Buffer )]),
   
    io:format("=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-~n~n"),
    io:format("Running parallel fill/pull test...~n"),
    time_run(fun parallel_test/2, [Buffer, 1]),
%   io:format("SIZE: ~p~n",[ libemp_buffer:size( Buffer )]),

    io:format("=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-~n~n"),
    io:format("Running parallel fill/pull test...~n"),
    time_run(fun parallel_test/2, [Buffer, 50]),   
%   io:format("SIZE: ~p~n",[ libemp_buffer:size( Buffer )]),
   
    halt(0).

%%% ===
%%% Time a test and print out statistics.
%%% ===

time_run( TestFun, Args ) ->
    io:format("========================================~n"),
    Test = erlang:apply(TestFun, Args), % Initialize test
    {Time, _} = timer:tc( Test ),
    io:format(<<"Test finished in: ~tp μs~n~n"/utf8>>,[Time]).

%%% ===
%%% Build our buffers using their default configurations:
%%% ===

run_drop() ->
    {ok, Args} = file:consult("src/buffers/libemp_drop_buffer.cfg"),
    {ok, Buff} = libemp_buffer:start( Args ),
    run_tests( Buff ).

run_simple() ->
    {ok, Args} = file:consult("src/buffers/libemp_simple_buffer.cfg"),
    {ok, Buff} = libemp_buffer:start( Args ),
    run_tests( Buff ).

run_epocxy() ->
    {ok, Args} = file:consult("src/buffers/libemp_epocxy_buffer.cfg"),
    {ok, Buff} = libemp_buffer:start( Args ),
    run_tests( Buff ).

run_batchets() ->
    {ok, Args} = file:consult("src/buffers/libemp_batchets_buffer.cfg"),
    {ok, Buff} = libemp_buffer:start( Args ),
    run_tests( Buff ).

%%% ===
%%% Run a test on the buffer. 
%%% ===

fillup_test( Buffer, ProducerCount ) ->
    Producers = make_procs( fun producer/4, Buffer, ProducerCount ),
    fun() -> start_procs( Producers ), hang_for( ProducerCount ) end.

pullout_test( Buffer, ConsumerCount ) -> 
    Consumers = make_procs( fun consumer/4, Buffer, ConsumerCount ),
    fun() -> start_procs( Consumers ), hang_for( ConsumerCount ) end.

parallel_test( Buffer, ProducerCount ) -> 
    Producers = make_procs( fun producer/4, Buffer, ProducerCount ),
    Consumers = make_procs( fun consumer/4, Buffer, 1  ),
    fun() -> 
        start_procs( Producers++Consumers ),
        hang_for( 1+ProducerCount )
    end.

%%% ===
%%% UTILITIES
%%% ===

make_procs(F, Buffer, N) ->
    Max = ?FILLUP_COUNT div N,
    Me = self(),
    lists:map( fun(I)->
                    Size = case 
                               N /= 1 andalso 
                               N rem 2 =:= 1 andalso 
                               I =:= 1 
                           of 
                               true -> Max+1; 
                               false -> Max
                           end,
                    spawn(fun()->init_proc(F,Buffer,I,Me,Size)end)
               end,
               lists:seq(1,N) ).

start_procs(Ps) -> lists:foreach(fun(P)->P!start end,Ps).

init_proc(F,Buffer,Id,Me,Start) ->
%    io:format("Starting Proc(~p), with init: ~p~n",[Id,Start]),
    receive start -> F(Buffer,Id,Me,Start) end.

producer(_,_,Parent,0) -> Parent!ok;
producer(Buffer,Id,P,Value) -> 
    libemp_buffer:give({event,Id,Value},Buffer),
    producer(Buffer,Id,P,Value-1).

consumer(_,_,Parent,N) when N =< 0 -> Parent!ok; % Race condition!
consumer(Buffer,Id,P,Value) ->
    Es = libemp_buffer:take(Buffer),
    Length = length(Es),
%    io:format("~p: ~p -> consuming: ~p~n",[libemp_buffer:size(Buffer),
%                                           Value, Length]),
    consumer(Buffer,Id,P,Value-Length).

hang_for( 0 ) -> ok;
hang_for( N ) -> receive ok -> hang_for( N-1 ) end.

