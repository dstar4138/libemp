%%% LibEMP Logger Buffer -
%%%
%%%     All events are logged via the error_logger and forgotten. Useful for 
%%%     potential Event Monitor testing. 
%%%
-module(libemp_logger_buffer).
-behaviour(libemp_buffer).

-export([initialize/1, register/2, destroy/1]).

initialize( Args ) ->
    {ok, LogFun} = get_logging_function( Args ),
    {ok, LogFun}.

register( _TakerGiver, LogFun ) ->
    libemp_buffer:create([
        {take,fun() -> [] end},
        {give, fun(Msg) -> catch erlang:apply(LogFun, ["Event ~p:",[Msg]]) end},
        {size, fun() -> 0 end},
        {unregister, fun() -> ok end}
    ]).

destroy( _ ) ->
    ok.

%%% ==========================================================================
%%% Private Functionality
%%% ==========================================================================

get_logging_function( Args ) ->
    case proplists:lookup(logger_fun, Args) of
        {logger_fun, Fun} when is_function(Fun,2) -> {ok, Fun};
        none ->{ok, fun error_logger:info_msg/2}
    end.
