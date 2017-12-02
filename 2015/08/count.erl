-module(count).
-compile(export_all).

lexer() -> "lexer_08".
parser() -> "parser_08_2".
part() -> part2.

start() ->
    Device = toolbox:get_device({file, "input"}),
    toolbox:prepare_parser(lexer(), parser()),
    {Hyper, Long, Short} = toolbox:fold_lines(fun handle_line/2, {0,0,0}, Device),
    case part() of
        part1 -> Long - Short;
        part2 -> Hyper - Long
    end.

handle_line(Line, {AccH, AccL, AccS}) ->
    {Hyper, Long, Short} = toolbox:parse_line(lexer(), parser(), Line),
    {Hyper+AccH, Long+AccL, Short+AccS}.

