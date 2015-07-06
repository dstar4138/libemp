%%% LibEMP   
%%% 
%%%

-module(libemp_app).
-behaviour(application).
-include("internal.hrl").

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @doc Called by `application' module to start up the LibEMP Erlang 
%%  OTP supervision tree.
%% @end
start(_StartType, StartArgs) -> 
    libemp_sup:start_link( StartArgs ). % start daemon

%% @doc Called right before stopping libemp.
prep_stop( _State ) -> 
    ?LOG("libemp stopping").

%% @doc Called after stopping libemp. Unused.
stop(_State) -> 
    ok.

