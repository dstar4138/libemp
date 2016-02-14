%%% LibEMP Application - 
%%% 
%%%     The root of the whole LibEMP OTP Application supervisory tree.
%%%
-module(libemp_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @doc Called by `application' module to start up the LibEMP Erlang 
%%  OTP supervision tree.
%% @end
start(_StartType, _StartArgs) -> 
    libemp_sup:start_link().

%% @doc Called after stopping libemp. Unused.
stop(_State) -> 
    ok.

