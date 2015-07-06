%%% LibEMP Utility Functionality -
%%% 
-module(libemp_util).

-export([merge_default_args/2]).

merge_default_args( Overrides, Args ) ->
    lists:keymerge(1, Overrides, proplists:get_value(default_args, Args, [])).

