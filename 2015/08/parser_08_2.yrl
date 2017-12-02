Nonterminals
Line List Character Hex AnyLetter.

Terminals
quote backslash letter digit af x.

Rootsymbol
Line.

Line -> quote List quote            : triple_add({6,2,0}, '$2').
List -> '$empty'                    : {0,0,0}.
List -> Character List              : triple_add('$1', '$2').
Character -> AnyLetter              : {1,1,1}.
Character -> backslash quote        : {4,2,1}.
Character -> backslash backslash    : {4,2,1}.
Character -> backslash x Hex Hex    : {5,4,1}.

Hex -> af                           : {0,0,0}.
Hex -> digit                        : {0,0,0}.
AnyLetter -> letter                 : {0,0,0}.
AnyLetter -> af                     : {0,0,0}.
AnyLetter -> x                      : {0,0,0}.


Erlang code.

tuple_add({A,B}, {X,Y}) -> {A+X, B+Y}.
triple_add({A,B,C}, {X,Y,Z}) -> {A+X, B+Y, C+Z}.
