-module(look_and_say).
-compile(export_all).

part() -> part2.

start() ->
    Input = "1113222113",
    List = lists:map(fun char_to_int/1, Input),
    Rep = case part() of
              part1 -> 40;
              part2 -> 50
          end,
    lists:foldl(fun(_, Acc) -> flat_rle(Acc) end, List, lists:seq(1,Rep)).

%% [1,1,2] -> [2,1, 1,2]
flat_rle(L) ->
    lists:flatten(lists:map(fun(T) -> tuple_to_list(T) end, rle(L))).

%% run length encoding
rle(L) -> rle(L, []).

rle([], Acc) -> lists:reverse(Acc);
rle([H|T], Acc) -> 
    L = 1 + length(lists:takewhile(fun(X) -> X==H end, T)),
    rle(lists:dropwhile(fun(X) -> X==H end, T), [{L,H}|Acc]).

char_to_int(C) when C >= $0, C =< $9 ->
    C - $0.
