-module(wires).
-compile(export_all).

part() -> part1.
lexer() -> "lexer_07".
parser() -> "parser_07".

start() ->
    Device = toolbox:get_device({file, "input"}),
    toolbox:prepare_parser(lexer(), parser()),
    WireValues = case part() of
                     part1 -> [];
                     part2 -> [{"b", 956}];
                     recursive -> []
                 end,
    WS_start = toolbox:fold_lines(fun handle_line/2, WireValues, Device),
    WS_final = case part() of 
        recursive -> define_wire("a", WS_start);
        _ -> toolbox:converge(fun update_wires/1, fun(WS) -> wire_is_defined("a", WS) end, WS_start)
               end,
    get_value("a", WS_final).


%% WireValues = [{"a", 42}, {"b", {"c",or_op,"d"}, ...}]
handle_line(Line, WireValues) ->
    {Assignment, Wire} = toolbox:parse_line(lexer(), parser(), Line),
    case lists:keyfind(Wire, 1, WireValues) of
        false -> lists:keystore(Wire, 1, WireValues, {Wire, Assignment});
        _ -> WireValues %% only the first assignment counts
    end. 
 

update_wires(WireValues) ->
    lists:map(fun(Tuple) -> update_single_wire(Tuple, WireValues) end, WireValues).

%% alternative:
%% define_wire(Wire, WireValues) -> 
%%     {Wire, Assignment} = find in WireValues
%%     case Assignment of
%%         {not_op, Arg} -> update Wire in WireValues to define_wire(Arg, WireValues)
%%         {Arg1, Op, Arg2} -> update to Op(define_wire(Arg1), define_wire(Arg2))
%%         {Arg1, Op, Arg2} -> W_tmp = define(Arg1, WireValues),
%%                             W_tmp2 = define(Arg2, W_tmp),
%%                             update to Op()
define_wire(Wire, WireValues) when is_integer(Wire) -> Wire.

update_single_wire({Wire, Assignment}, WireValues) -> 
    case Assignment of
        {not_op, Arg} -> 
            NewVal = get_value_maybe(Arg, WireValues),
            case NewVal of
                undefined -> {Wire, Assignment};
                _ -> {Wire, not_16b(NewVal)}
            end;
            
        {Arg1, Op, Arg2} -> 
            F = case Op of
                    and_op -> fun and_16b/2;
                    or_op -> fun or_16b/2;
                    lshift_op -> fun lshift_16b/2;
                    rshift_op -> fun rshift_16b/2
                end,
            NewVal1 = get_value_maybe(Arg1, WireValues),
            NewVal2 = get_value_maybe(Arg2, WireValues),
            case NewVal1 of
                undefined -> {Wire, Assignment};
                _ -> case NewVal2 of
                         undefined -> {Wire, Assignment};
                         _ -> 
                             {Wire, F(NewVal1, NewVal2)}
                     end
            end;

        Arg ->
            NewVal = get_value_maybe(Arg, WireValues),
            case NewVal of
                undefined -> {Wire, Assignment};
                _ -> {Wire, NewVal}
            end
    end.

not_16b(X) ->
    (bnot X) band 65535.

and_16b(X,Y) ->
    (X band Y) band 65535.

or_16b(X,Y) ->
    (X bor Y) band 65535.

lshift_16b(X,N) ->
    (X bsl N) band 65535.

rshift_16b(X,N) ->
    (X bsr N) band 65535.

wire_is_defined(Wire, WireValues) ->
    %% {value, {"c", 42}} -> 42 -> true
    case lists:keysearch(Wire, 1, WireValues) of
        {value, V} when is_tuple(V) -> is_integer(element(2, V));
        _ -> false
    end.

get_value(Wire, WireValues) ->
    {Wire, Value} = lists:keyfind(Wire, 1, WireValues),
    Value.

get_value_maybe(Arg, WireValues) ->
    case is_list(Arg) of
        true -> case wire_is_defined(Arg, WireValues) of
                    true -> get_value(Arg, WireValues);
                    false -> undefined
                end;
        false when is_integer(Arg) -> 
            Arg
    end.

set_value(Wire, Value, WireValues) ->
    lists:keyreplace(Wire, 1, WireValues, Value).
