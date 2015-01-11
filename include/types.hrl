%% Public Types are exported through the libemp module for easy
%% access. All types included here are for internal type checking
%% purposes.

%% Plugin Types
-type plugin_name()   :: string().
-type plugin_module() :: atom().
-type plugin_action() :: atom().
-type plugin_ref()    :: plugin_module() | plugin_name().

%% Event Objects which are multicasted throughout the brokerage system.
-type libemp_event_name()  :: atom().
-type libemp_event_attr()  :: tuple().
-type libemp_event_attrs() :: [libemp_event_attr()]. %%TODO: compare w/ R17 map

%% Subscription representations:
%%  Created using sets of predicates to attribute names.
-type predicate_selector() :: atom().
-type predicate_op() :: '=='|'/='|'=<'|'<'|'>='|'>'|'=:='|'=/='.
-type predicate_const() :: {c@, atom()}. % Globals#globals.<constant>
-type predicate_value() :: {v@, predicate_selector()}. % Event#event.<value>
-type predicate_map() :: { [ predicate_selector() ], string() }.
-type predicate_mapping() :: true | [ predicate_map() ].

%% Utility types for use throughout
-type possible_error( FailureMessageType ) :: ok | {error, FailureMessageType}.
-type success_error( Success, Error ) :: {ok, Success} | {error, Error}.

