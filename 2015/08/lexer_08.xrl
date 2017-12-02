Definitions.

WS = ([\000-\s]|%.*)
L = [G-Zg-wyz]
X = x
D = [0-9]
AF = [a-fA-F]
Q = "
B = \\

Rules.

{Q}     : {token, {quote, TokenLine, TokenChars}}.
{B}     : {token, {backslash, TokenLine, TokenChars}}.
{L}     : {token, {letter, TokenLine, TokenChars}}.
{D}     : {token, {digit, TokenLine, TokenChars}}.
{AF}    : {token, {af, TokenLine, TokenChars}}.
{X}     : {token, {x, TokenLine, TokenChars}}.
{WS}+   : skip_token.

Erlang code.

