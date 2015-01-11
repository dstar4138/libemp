%%% libemp_subscription.erl - Subscription Object Management
%%%
%%% Subscriptions are at the core of EMP. This module provides the mechanics to
%%% parse and manipulate the Subscription Objects during runtime. These objects
%%% are passed to the unification system for recompilation, this mechanism is
%%% separate from Subscription object management. 
%%%
-module(libemp_subscription).

-include("internal.hrl").
-define(DEFAULT_PREDICATE,true).

-export( [parse/1, to_sublang/1] ).
-export( [new/3] ).

%% @doc From a string, parse out the subscription objects according to 
%%    the EMP Subscription Language. Note multiple subscriptions can be
%%    one after another to be passed in all at once.
%% @end
%% @reference docs/EMPSubscriptionLang.md
-spec parse( string() ) -> success_error([libemp_subscription()], any()).
parse( String ) -> ok.


%% @doc Convert the internal subscription object into its textual 
%%   representation via the subscription language. Note multiple subscriptions
%%   can be encoded at once, thus useful for complete database dumps. 
%% @end
-spec to_sublang( [libemp_subscription()] ) -> success_error(string(), any()).
to_sublang( _Subs ) -> ok.


%% @doc Used to wrap the creation of internal subscription objects, in case its
%%  definition changes.
%% @end
-spec new( libemp_event_name(), predicate_mapping(), string_code() ) -> libemp_subscription(). 
new( EventName, Predicate, ActionSet ) ->
    #libemp_subscription{ event = EventName, 
                          predicate = Predicate, 
                          action = ActionSet }.


%% @doc Called via parsing, will list the global values avaliable for use in 
%%   predicates and action sets.
%% @end
-spec globals() -> [ atom() ].
globals() -> []. %TODO: Get from DB?
