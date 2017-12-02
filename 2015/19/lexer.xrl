Definitions.

WS = ([\000-\s]|%.*)
LC = [A-Z]
L = [A-Za-z]
D = [0-9]


Rules.

to      : {token, {to, TokenLine, TokenChars}}.
{LC}+{L}* : {token, {word, TokenLine, TokenChars}}.
{D}+    : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
=       : {token, {equal, TokenLine, TokenChars}}.
{WS}+   : skip_token.

Erlang code.

