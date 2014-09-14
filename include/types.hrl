%% Public Types are exported through the libemp module for easy
%% access. All types included here are for internal type checking
%% purposes.

%% Event Objects which are multicasted throughout the brokerage system.
-type libemp_event_name() :: atom().
-type libemp_event_attr() :: tuple().%map(). %%TODO: switch to R17
-type libemp_event() :: {event, libemp_event_name(),
                                libemp_event_attr()}.


%% Subscription representations:
%%  Created using sets of predicates to attribute names.
-type predicate_op() :: '=='|'/='|'=<'|'<'|'>='|'>'|'=:='|'=/='.
-type predicate_map() :: { atom(), predicate_op(), term() }.
