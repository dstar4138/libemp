%%% LibEMP Event Processor Supervisor -
%%%
%%%     Event Processors encapsulate a stack of LibEMP Event Sinks, and a Buffer
%%%     reference. If a Processor fails, it finds its Buffer reference
%%%     again and builds the Sink Stack. The Processors run the work pull loop
%%%     and can be customized by which Stack they build, which Buffer they are
%%%     attached to, and how many Processor processes are running on each 
%%%     machine.
%%%
-module(libemp_processor_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, add_processor/3]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor, then enables the set of Processors 
%%   requested.
%% @end
start_link() ->
    case
        supervisor:start_link({local, ?MODULE}, ?MODULE, [])
    of
        ignore -> ignore;
        {error,_}=E -> E;
        {ok,_}=Res ->
            % On success, initialize processors, and return Result.
            initialize_processors(),
            Res
    end.

%% @doc Build and add a processor to the supervisor. It will immediately
%%   begin pulling from the buffer.
%% @end 
add_processor( BufferName, Handler, StackConfig ) ->
    case
        libemp_processor:validate_configs( BufferName, Handler, StackConfig )
    of
        ok ->
            supervisor:start_child( ?MODULE, [BufferName,Handler,StackConfig] );
        {error,_}=Error -> 
            Error
    end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Initialize the supervisor as a simple-one-for-one style monitor.
init( _ ) ->
    %TODO: Pull out shutdown setting from config. Along with intensity/period.
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => processor,
                    restart => permanent,
                    start => {libemp_processor, start_link, []},
                    shutdown => brutal_kill}],
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%% @doc Get the currently initialized buffer and link all of them processors
%%   that we start up.
%% @end 
initialize_processors( ) ->
    lists:foreach( fun({BufferName,Handler,StackConfig}) ->
        add_processor(BufferName,Handler,StackConfig)
    end, libemp_util:get_cfgs( stacks ) ).

