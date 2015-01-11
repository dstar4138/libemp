%%% libemp_subscription_parser.yrl -
%%%
%%%     Utilizing the scanned in tokens, convert them to a 
%%%
%%%

Nonterminals
    subscriptions subscription
    eventname predicates actionset
    pecs_with_vars pecs vars var
.

Terminals
    op_on op_when op_trigger op_end
    atom event global pec star
.

Rootsymbol subscriptions.
Endsymbol '$end'.

%% ===========================================================================

subscriptions -> subscription subscriptions : [ '$1' | '$2' ].
subscriptions -> subscription : [ '$1' ].

subscription -> op_on eventname op_when predicates op_trigger actionset op_end:
                    build_subscription( '$1', '$3', '$5' ).
subscription -> op_on eventname op_trigger actionset op_end: 
                    build_subscription( '$1', true, '$5' ).

eventname -> atom : {event, value_of('$1')}.
eventname -> star : all.

predicates -> pecs_with_vars : '$1'.
actionset  -> pecs_with_vars : '$1'.

pecs_with_vars -> pecs vars pecs_with_vars : [ '$1' | '$2' ] ++ '$3'.
pecs_with_vars -> pecs vars : [ '$1' | '$2' ].

pecs -> pec pecs : value_of( '$1' ) ++ '$2'.
pecs -> pec : value_of( '$1' ).
pecs -> star : "*".

vars -> var vars :  [ '$1' | '$2' ]. 
vars -> var : [ '$1' ].

var -> event  : {event, value_of( '$1' )}.
var -> global : {global, value_of( '$1' )}.

%% ===========================================================================
Erlang code.
%% ===========================================================================

%% @doc Builds a subscription object that can be used to build or update the 
%%   unifier. Does so by subsequently parsing the guards and erlang code.
%% @end
build_subscription( EventName, PredicateList, ActionSet ) ->
    ValueList = libemp_event:values( EventName ),
    Globals = libemp_subscription:globals(),
    BuilderFunction = build_value_selector( EventName, ValueList, Globals ), 
    PL = lists:map( BuilderFunction, PredicateList ),
    AS = lists:map( BuilderFunction, ActionSet ),
    Predicate = parse_predicate( PL ),
    libemp_subscription:new( EventName, Predicate, AS ).
   
%% @private
%% @doc Build source erlang code for selecting either a global or event values 
build_value_selector( EventName, ValueList, Globals ) -> 
    % Function returned to perform action on all Value Names
    fun 
        % $EVENT or #EVENT both mean the current event object, which will be
        % a variable called L_Event (or local event).
        ( 'EVENT' ) -> "L_Event";
        
        % If it's unknown (which only happens in DEBUG), just replace with an
        % atom. This will aid in debugging when we look at the parsed Erlang
        % code, without causing errors.
        ( {unknown, ValueName} ) -> io_lib:format("~p", ValueName);
        
        % Otherwise, check if it's an Event variable. If it is, then we'll 
        % select it from the event itself via
        ( {event, AttrName} ) -> 
            case proplists:lookup( AttrName, ValueList ) of
                { Value, Num } -> io_lib:format("element(~d, Event)",[Num]);
                none ->
                    ErrorMsg = "Unknown Event Attribute name: ~p",
                    error( io_lib:format( ErrorMsg, AttrName ) )
            end;
        ( {global, ValueName} ) -> 
            case lists:member( ValueName, Globals ) of
                true  -> 
                    "G_"++string:to_upper( erlang:atom_to_list( ValueName ) );
                false -> 
                    ErrorMsg = "Unknown Global variable: ~p",
                    error( io_lib:format( ErrorMsg, ValueName ) )
            end
    end.

