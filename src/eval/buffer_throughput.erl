%%% Buffer testing
-module(buffer_throughput).
-export([run/1,run_tests/1,reductions/1]).
-export([take/4,give/4]).
-define(FILLUP_COUNT, 1000000).

%% @doc Run a set of tests over the buffer, namely agressively adding/removing
%%   events with various numbers of producers/consumers.
%% @end
run_tests( QueueName ) ->
    {ok, BufferArgs} = get_configs( QueueName ),
    
    io:format("Running ~p fill up test with ~p producers...~n",[?FILLUP_COUNT, 1]),
    {ok, Buffer1} = make_buffer( BufferArgs ),
    time_run(fun fillup_test/2, [Buffer1,1]),

    timer:sleep(1000),
    io:format("=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-~n~n"),
    io:format("Running ~p pull out test with ~p consumers...~n",[?FILLUP_COUNT, 1]),
    time_run(fun pullout_test/2, [Buffer1,1]),
    libemp_buffer:destroy( Buffer1 ),

    timer:sleep(1000),
    io:format("=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-~n~n"),
    io:format("Running parallel fill (1) / pull (1) test...~n"),
    {ok, Buffer2} = make_buffer( BufferArgs ),
    time_run(fun parallel_test/2, [Buffer2, 1]),
    libemp_buffer:destroy( Buffer2 ),

    timer:sleep(1000),
    io:format("=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-~n~n"),
    io:format("Running parallel fill (50) / pull (1) test...~n"),
    {ok, Buffer3} = make_buffer( BufferArgs ),
    time_run(fun parallel_test/2, [Buffer3, 50]),
    libemp_buffer:destroy( Buffer3 ).   

%% @doc Run the tests and then halt the VM.
run( QueueName ) ->
    run_tests( QueueName ),
    halt( 0 ).

%% @doc Pass in the number of times to run, and get the number of reductions 
%%   it takes to push an item onto a buffer. My runs suggest roughly 3.
%% @end
reductions(Count) ->
    {ok, BufferArgs} = get_configs("drop"),
    {ok, InitBuffer} = make_buffer( BufferArgs ),
    {ok, Buffer} = libemp_buffer:register( give, InitBuffer ),
    GVals = lists:foldl( fun( _, App ) -> 
                         Self = self(),
                         CurCount = element(2,process_info(Self,reductions)),
                         libemp_buffer:give(0, Buffer),
                         NxtCount = element(2,process_info(Self,reductions)),
                         [NxtCount-CurCount|App]
            end, [], lists:seq(1,Count) ),
    % Prune first couple itterations to avoid issues with startup.
    CleanSize = round(max(1,Count-(Count/5))),
    {GValsClean,_} = lists:split(CleanSize, GVals),
    io:format("GIVE: Avgs ~p, Max ~p, Min ~p~n~p~n",[lists:sum(GValsClean)/CleanSize,
                                           lists:max(GValsClean),
                                           lists:min(GValsClean),
                                           lists:sort(GValsClean)]),
    TVals = lists:foldl( fun( _, App ) -> 
                         Self = self(),
                         CurCount = element(2,process_info(Self,reductions)),
                         libemp_buffer:take(Buffer),
                         NxtCount = element(2,process_info(Self,reductions)),
                         [NxtCount-CurCount|App]
            end, [], lists:seq(1,Count) ),
    {TValsClean,_} = lists:split(CleanSize, TVals),
    io:format("TAKE: Avgs ~p, Max ~p, Min ~p~n~p~n",[lists:sum(TValsClean)/CleanSize,
                                           lists:max(TValsClean),
                                           lists:min(TValsClean),
                                           lists:sort(TValsClean)]),
    ok.

%%% ===
%%% Time a test and print out statistics.
%%% ===

time_run( TestFun, Args ) ->
    io:format("========================================~n"),
    Test = erlang:apply(TestFun, Args), % Initialize test
    {Time, _} = timer:tc( Test ),
    io:format("Test finished in: ~tp μs~n~n",[Time]).

%%% ===
%%% Run a test on the buffer. 
%%% ===

fillup_test( Buffer, ProducerCount ) ->
    Producers = make_procs( give, Buffer, ProducerCount ),
    fun() -> start_procs( Producers ), hang_for( ProducerCount ) end.

pullout_test( Buffer, ConsumerCount ) ->
    Consumers = make_procs( take, Buffer, ConsumerCount ),
    fun() -> start_procs( Consumers ), hang_for( ConsumerCount ) end.

parallel_test( Buffer, ProducerCount ) ->
    Producers = make_procs( give, Buffer, ProducerCount ),
    Consumers = make_procs( take, Buffer, 1  ),
    fun() -> 
        start_procs( Producers++Consumers ),
        hang_for( 1+ProducerCount )
    end.

%%% ===
%%% UTILITIES
%%% ===

get_configs( QueueName ) ->
    Module = ["libemp_", QueueName,"_buffer"],
    ExPath = lists:flatten(["../src/buffers/",QueueName,"/",Module,".cfg"]),
    case file:consult(ExPath) of
        {error, enoent} ->
            NewPath = lists:flatten(["../src/buffers/testing/",Module,".cfg"]),
            io:format(NewPath),io:nl(),
            file:consult(NewPath);
        {error,_}=E -> E;
        Val -> 
            io:format(ExPath),io:nl(),
            Val
    end.

make_buffer( BufferArgs ) ->
    case libemp_buffer:start( BufferArgs ) of
        {ok, Buffer} -> {ok, Buffer};
        {ok, _, Buffer} -> {ok, Buffer};
        Err -> Err
    end.

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

init_proc(Direction,BufferInit,Id,Me,Start) ->
%    io:format("Starting Proc(~p), with init: ~p~n",[Id,Start]),
    {ok, Buffer} = libemp_buffer:register( Direction, BufferInit ),
    receive start -> erlang:apply(?MODULE, Direction, [Buffer,Id,Me,Start]) end.

give(_,_,Parent,0) -> Parent!ok;
give(Buffer,Id,P,Value) -> 
    libemp_buffer:give({event,Id,Value},Buffer),
    give(Buffer,Id,P,Value-1).

take(_,_,Parent,N) when N =< 0 -> Parent!ok; % Race condition!
take(Buffer,Id,P,Value) ->
    Es = libemp_buffer:take(Buffer),
    Length = length(Es),
%    io:format("~p: ~p -> consuming: ~p~n",[libemp_buffer:size(Buffer),
%                                           Value, Length]),
    take(Buffer,Id,P,Value-Length).

hang_for( 0 ) -> ok;
hang_for( N ) -> receive ok -> hang_for( N-1 ) end.

