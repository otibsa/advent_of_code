-module(aunt_sue).
-compile(export_all).

-define(PART, part2).

start() ->
    Sample = [{children, 3}, {cats, 7}, {samoyeds, 2}, {pomeranians, 3}, {akitas, 0}, {vizslas, 0}, {goldfish, 5}, {trees, 3}, {cars, 2}, {perfumes, 1}],
    Device = toolbox:get_device({file, "input"}),
    Acc = {Sample, []},
    {_,Nr} = toolbox:fold_lines(fun handle_line/2, Acc, Device),
    io:format("~w~n", [Nr]).

handle_line(Line, {Sample, Matches}) ->
    Regex = "^Sue (.*?): (.*?): (.*?), (.*?): (.*?), (.*?): (.*?)$",
    [AuntNrS, P1S, V1S, P2S, V2S, P3S, V3S] = toolbox:match_line(Line, Regex),
    AuntNr = list_to_integer(AuntNrS),
    P1 = list_to_atom(P1S),
    P2 = list_to_atom(P2S),
    P3 = list_to_atom(P3S),
    V1 = list_to_integer(V1S),
    V2 = list_to_integer(V2S),
    V3 = list_to_integer(V3S),
    case matching([{P1,V1},{P2,V2},{P3,V3}], Sample) >= 3 of
        true -> {Sample, [{AuntNr,{P1,V1}, {P2,V2}, {P3,V3}}|Matches]};
        false -> {Sample, Matches}
    end.

%% AuntProp = [{},{},{}]
%% Sample =   [{},{},{},{},{},{},{},{},{},{}]
matching(AuntReadings, Sample) ->
    lists:foldl(
      fun(PV, Acc) ->
              Acc + match_prop(PV, Sample)
      end, 0, AuntReadings).

match_prop({P,V_read}, Sample) ->
    Known = lists:keyfind(P, 1, Sample),
    case ?PART of
        part1 -> 
            case Known of
                {P,V_read} -> 1;
                _ -> 0
            end;
        part2 ->
            case Known of
                {cats, V_known} when V_read > V_known -> 1;
                {cats, V_read} -> 0;
                {trees, V_known} when V_read > V_known -> 1;
                {trees, V_read} -> 0;
                {pomeranians, V_known} when V_read < V_known -> 1;
                {pomeranians, V_read} -> 0;
                {goldfish, V_known} when V_read < V_known -> 1;
                {goldfish, V_read} -> 0;
                {P,V_read} -> 1;
                _ -> 0
            end
    end.
