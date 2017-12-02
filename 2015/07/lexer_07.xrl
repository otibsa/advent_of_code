Definitions.

D = [0-9]
LS = [a-z]
LB = [A-Z]
WS = ([\000-\s]|%.*)
A = ->

Rules.

AND     : {token, {and_op, TokenLine, list_to_atom(TokenChars)}}.
OR      : {token, {or_op, TokenLine, list_to_atom(TokenChars)}}.
NOT     : {token, {not_op, TokenLine, list_to_atom(TokenChars)}}.
LSHIFT  : {token, {lshift_op, TokenLine, list_to_atom(TokenChars)}}.
RSHIFT  : {token, {rshift_op, TokenLine, list_to_atom(TokenChars)}}.
{D}+    : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{LS}+   : {token, {wire, TokenLine, TokenChars}}.
{A}     : {token, {ass_op, TokenLine, list_to_atom(TokenChars)}}.
{WS}+   : skip_token.

Erlang code.

atom(TokenChars) -> list_to_atom(TokenChars).
