%%% LibEMP Drop Buffer -
%%%
%%%     A useless buffer which forgets all events that you give it. Used to
%%%     test the overhead the LibEMP Buffer framework adds. 
%%%
-module(libemp_drop_buffer).
-behaviour(libemp_buffer).
-export([initialize/1,register/2]).

initialize( _ ) -> 
    {ok, []}.

register( _, _ ) -> 
    libemp_buffer:create([
        {take, fun() -> [] end},
        {give, fun(_)-> ok end},
        {size, fun() -> 0 end},
        {destroy, fun() -> ok end}
    ]).
