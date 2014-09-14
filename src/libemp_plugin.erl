%%% This module defines the libemp Plug-in behaviour.
-module(libemp_plugin).

%% Tell EMP what events this plug-in is capable of generating in the future.
%% libemp will optimize the subscription and unification trees based on this.
-callback advertise_events() -> [ libemp:event() ].

%% Registering your actions, subscriptions can now trigger commands which 
%% activate your actions.
-callback advertise_actions() -> [ libemp:action() ].

%% Allows for the freezing of time sensitive sub-processes. But otherwise
%%  unimportant.
-callback pause( State :: any() ) -> ok.
-callback unpause( State :: any() ) -> ok.


%% Default Behaviours
-export([unify/1,evaluation_tree/0]).
%% libemp - System generated functions:
% 
% unify( libemp:event() ) -> ok.
%   * For each subscription, this function is mutated and reloaded to
%   futher speed up event routing.
unify( _ ) -> ok.

% evaluation_tree() -> libemp:eval_tree().
%   * A full evaluaton tree to help with unify/1 generation. This keeps
%   the full pattern matching tree in it's current state for ease of 
%   update with new subscriptions. The goal is to build the smallest 
%   tree of matches which will evaluate to a correct route.
%
evaluation_tree() -> [].




