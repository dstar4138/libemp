%%% LibEMP Logger Buffer -
%%%
%%%     All events are logged via the error_logger and forgotten. Useful for 
%%%     potential Event Monitor testing. 
%%%
-module(libemp_logger_buffer).
-behaviour(libemp_buffer).

-export([initialize/1, register/2, destroy/1]).

initialize( _ ) -> 
    {ok, []}.

register( _, _ ) ->
    libemp_buffer:create([
        {take,fun() -> [] end},
        {give, fun(Msg) -> error_logger:info_msg("Event ~p:",[Msg]) end},
        {size, fun() -> 0 end},
        {unregister, fun() -> ok end}
    ]).

destroy( _ ) ->
    ok.
