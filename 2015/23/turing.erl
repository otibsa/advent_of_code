-module(turing).
-compile(export_all).

-define(PART, part2).

start() ->
    Device = toolbox:get_device({file, "input"}),
    Program = toolbox:fold_lines(fun handle_line/2, [], Device),
    execute_program(Program).

execute_program(Program) ->
    case ?PART of
        part1 -> next_instruction(0,0,0,Program);
        part2 -> next_instruction(1,0,0,Program)
    end.

next_instruction(Ra,Rb,PC,Program) ->
    case orddict:find(PC,Program) of
        error -> {Ra,Rb,PC};
        {ok, Instruction} ->
            {NextRa,NextRb,NextPC} =
                case Instruction of
                    {hlf, a} -> {Ra div 2,Rb,PC+1};
                    {hlf, b} -> {Ra,Rb div 2,PC+1};
                    {tpl, a} -> {Ra*3,Rb,PC+1};
                    {tpl, b} -> {Ra,Rb*3,PC+1};
                    {inc, a} -> {Ra+1,Rb,PC+1};
                    {inc, b} -> {Ra,Rb+1,PC+1};
                    {jmp, Offset} -> {Ra,Rb,PC+Offset};
                    {jie, a, Offset} ->
                        case Ra rem 2 of
                            0 -> {Ra,Rb,PC+Offset};
                            _ -> {Ra,Rb,PC+1} 
                        end;
                    {jie, b, Offset} ->
                        case Rb rem 2 of
                            0 -> {Ra,Rb,PC+Offset};
                            _ -> {Ra,Rb,PC+1} 
                        end;
                    {jio, a, Offset} ->
                        case Ra of
                            1 -> {Ra,Rb,PC+Offset};
                            _ -> {Ra,Rb,PC+1} 
                        end;
                    {jio, b, Offset} ->
                        case Rb of
                            1 -> {Ra,Rb,PC+Offset};
                            _ -> {Ra,Rb,PC+1} 
                        end
                end,
            next_instruction(NextRa, NextRb, NextPC, Program)
    end.


handle_line(Line, Program) ->
    Regex = "(.*?) (.*)",
    [Cmd, Op] = toolbox:match_line(Line, Regex),
    X = case string:tokens(Op, ", ") of
        [Op1] when Cmd =:= "jmp" ->
            {list_to_atom(Cmd), list_to_integer(Op1)};
        [Op1] ->
            {list_to_atom(Cmd), list_to_atom(Op1)};
        [Op1,Op2] ->
            {list_to_atom(Cmd), list_to_atom(Op1), list_to_integer(Op2)}
    end,
    orddict:store(orddict:size(Program), X, Program).

