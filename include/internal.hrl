%% Internal Headers consistent across the entire EMP Library
-include("debug.hrl").
-include("logging.hrl").
-include("types.hrl").

%% Internal Object representations
%%% Subscriptions
-type string_code() :: string().
-record( libemp_subscription, { priority = 0 :: pos_integer(),
                                event     :: libemp_event_name(),
                                predicate :: predicate_mapping(),
                                action    :: string_code() }).
-type libemp_subscription() :: #libemp_subscription{}.

%%% Events
-record( libemp_event, { name       :: libemp_event_name(),
                         attributes :: libemp_event_attrs() }).
-type libemp_event() :: #libemp_event{}.

