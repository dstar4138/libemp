%%% 
%%% Utility functionality used in various places throughout the library.
%%%
-module( libemp_util ).

%% Public functionality
-export([concat_atoms/1]).

%% Concatenates all parts of a set of atoms and returns the full concatenation.
-spec concat_atoms( [ atom() ] ) -> atom().
concat_atoms([]) -> exit(badarg);
concat_atoms([H]) -> H;
concat_atoms([H|T]) -> list_to_atom( atom_to_list(H) ++ 
                                     atom_to_list(concat_atoms(T)) ).

