-module(template).
-compile(export_all).

-define(PART, part1).

%% lexer() -> "lexer".
%% parser() -> "parser".

start() ->
    Device = toolbox:get_device({file, "input"}),
    %% toolbox:prepare_parser(lexer(), parser()),
    Acc = [],
    toolbox:fold_lines(fun handle_line/2, Acc, Device).

handle_line(Line, Acc) ->
    %% ParseTree = toolbox:parse_line(Line).
    Regex = "(foo.*?) (bar.*?)",
    [Foo, Bar] = toolbox:match_line(Line, Regex),
    Acc.
