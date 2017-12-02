Nonterminals
Line List Character Hex AnyLetter.

Terminals
quote backslash letter digit af x.

Rootsymbol
Line.

Line -> quote List quote            : tuple_add({2,0}, '$2').
List -> '$empty'                    : {0,0}.
List -> Character List              : tuple_add('$1', '$2').
Character -> AnyLetter              : {1,1}.
Character -> backslash quote        : {2,1}.
Character -> backslash backslash    : {2,1}.
Character -> backslash x Hex Hex  : {4,1}.
AnyLetter -> letter                 : {0,0}.
AnyLetter -> af                     : {0,0}.
AnyLetter -> x                      : {0,0}.
Hex -> af                           : {0,0}.
Hex -> digit                        : {0,0}.


Erlang code.

tuple_add({A,B}, {X,Y}) -> {A+X, B+Y}.
letter_length({letters, TokenLine, TokenChars}) ->
    L = length(TokenChars),
    {L,L}.
