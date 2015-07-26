%%% LibEMP Utility Functionality -
%%% 
-module(libemp_util).

-export([merge_default_args/2]).

merge_default_args( Overrides, Args ) ->
    CleanOverrides = lists:keysort( 1, Overrides ),
    CleanArgs = lists:keysort( 1, proplists:get_value(default_args, Args, []) ),
    lists:keymerge(1, CleanOverrides, CleanArgs).

