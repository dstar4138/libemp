%%% LibEMP Monitor Object Wrapper -
%%%
%%%     This module defines the structure of an EMP Monitor such that LibEMP
%%%     knows how to comunicate with it.
%%%
-module(libemp_monitor).
-include("libemp.hrl").

-export([initialize/1]).

%% The Monitor's instance configuration.
-type libemp_monitor_config() :: ok.

%% The state of the monitor after initialization. Maintained by either the
%% Monitor Driver or the state server.
-type libemp_monitor_state() :: ok.


%% @doc Initialize the monitor instance and return the state of the system. 
-spec initialize( libemp_monitor_config() ) -> libemp_monitor_state(). 
initialize( MonitorInstanceConfig ) -> ok.






