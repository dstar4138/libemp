%%% LibEMP Application - 
%%% 
%%%     The root of the whole LibEMP OTP Application supervisory tree.
%%%     Please consult the application configuration (priv/*.config) to 
%%%     learn more how on LibEMP starts up for different use-cases.
%%%
-module(libemp_app).
-behaviour(application).
-include("internal.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @doc Called by `application' module to start up the LibEMP Erlang 
%%  OTP supervision tree.
%% @end
start(_StartType, _StartArgs) -> 
    ?LOG("LibEMP Starting up with Config:~n~p~n",[
        application:get_all_env( libemp )
    ]),
    libemp_sup:start_link().

%% @doc Called after stopping libemp. Unused.
stop(_State) -> 
    ok.

