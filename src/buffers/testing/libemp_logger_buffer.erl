%%% LibEMP Logger Buffer -
%%%
%%%     All events are logged via the error_logger for potential 
%%%     savings testsing.
%%%
-module(libemp_logger_buffer).
-behaviour(libemp_buffer).

-export([initialize/1, register/2]).

initialize( _ ) -> 
    {ok, []}.

register( _, _ ) ->
    libemp_buffer:create([
        {take,fun() -> [] end},
        {give, fun(Msg) -> error_logger:info_msg("Event: ~p",Msg) end},
        {size, fun() -> 0 end},
        {destroy, fun() -> ok end}
    ]).
