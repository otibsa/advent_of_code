Nonterminals
Line.

Terminals
integer word equal to.

Rootsymbol
Line.

Line -> word to word equal integer : {unwrap('$1'), unwrap('$3'), unwrap('$5')}.


Erlang code.

unwrap({_,_,V}) -> V.
