Nonterminals
Line Left Var.

Terminals
and_op or_op not_op lshift_op rshift_op wire integer ass_op.

Rootsymbol
Line.

Line -> Left ass_op wire        : {'$1', unwrap('$3')}.
Left -> Var                     : '$1'.
Var -> wire                     : unwrap('$1').
Var -> integer                  : unwrap('$1').
Left -> Var or_op Var           : {'$1', or_op, '$3'}.
Left -> Var and_op Var          : {'$1', and_op, '$3'}.
Left -> Var lshift_op integer   : {'$1', lshift_op, unwrap('$3')}.
Left -> Var rshift_op integer   : {'$1', rshift_op, unwrap('$3')}.
Left -> not_op Var              : {not_op, '$2'}.


Erlang code.

unwrap({_,_,V}) -> V.
