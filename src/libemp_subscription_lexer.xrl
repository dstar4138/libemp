%%% libemp_subscription_scanner.xrl - 
%%%
%%%     Tokenizes an input string according to the EMP Subscription language.
%%%  The subscription language can be summarized as following:
%%% 
%%%  Subscriptions ::= <Subscription [ Subscriptions ]
%%%  Subscription  ::= ON <EventName> [ WHEN <Predicates> ] TRIGGER <ActionSet>.
%%%  EventName     ::= [a-z][a-zA-Z0-9_@]+ # e.g. An atom.
%%%  Predicates    ::= <Predicate> [ (AND|OR) <Predicates> ]
%%%  Predicate     ::= #e.g. An Erlang Guard, $x represents an event value.
%%%  ActionSet     ::= #e.g. Erlang Code, $x are event values, #x are globals.
%%%

%% ===========================================================================
Definitions.
%% ===========================================================================

% Reserved Keywords %
On      = (o|O)(n|N)
When    = (w|W)(h|H)(e|E)(n|N)
Trigger = (t|T)(r|R)(i|I)(g|G)(g|G)(e|E)(r|R)

% Utility Matchers %
Atom = [a-z][a-ZA-Z0-9_@]*
Star = \*
WS   = (\n|\s|\t|\r)*
PotentialErlangCode = [^\$\#\n\s\t\r]+

%% ===========================================================================
Rules.
%% ===========================================================================

{On}      : mktoken( op_on, TokenLine, TokenChars ).
{When}    : mktoken( op_when, TokenLine, TokenChars ).
{Trigger} : mktoken( op_trigger, TokenLine, TokenChars ).
\.{WS}    : mktoken( op_end, TokenLine, TokenChars ). 
{Atom}    : mktoken( atom, TokenLine, TokenChars ).
{Star}    : mktoken( star, TokenLine, TokenChars ).

% Event and Global Value Selectors
\${Atom}  : mkvar_token( event, TokenLine, TokenChars, TokenLen ).
\#{Atom}  : mkvar_token( global, TokenLine, TokenChars, TokenLen ).

% Filter out all whitespace when we can.
{WS}      : skip_token.

% Everything left over is potentially Erlang code. We'll reconstruct in the 
% parser, but for now build it into a token for easy recognition. This could
% be much more expressive if we had our own Erlang Guard Expression lexer.
{PotentialErlangCode} : mktoken( pec, TokenLine, TokenChars ).

%% ===========================================================================
Erlang code.
%% ===========================================================================

%% In DEBUG mode, we'll allow unknown variables. This aids in lexer testing.
-ifdef(DEBUG).
-define(alert_or_allow(T,L,V), {token, {T, L, {unknown, V}}}).
-else.
-define(alert_or_allow(_,_,V),
            {error, "Unknown variable '"++V++"', missing in Atom table."}).
-endif. 

mktoken( Type, Line, Chars ) -> {token, {Type, Line, Chars}}.

mkvar_token( Type, Line, Chars, Length ) -> 
    VarName = lists:sublist( Chars, 1, Length ),
    case catch erlang:list_to_existing_atom( VarName ) of
        {'EXIT', _} -> ?alert_or_allow( Type, Line, VarName );
        VarAtom -> {token, {Type, Line, VarAtom}}
    end.

