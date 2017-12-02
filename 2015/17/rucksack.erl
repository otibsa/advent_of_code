-module(rucksack).
-compile(export_all).

-define(PART, part2).
-define(RUCKSACK, 150).

start() ->
    Device = toolbox:get_device({file, "input"}),
    Acc = [],
    Containers = toolbox:fold_lines(fun handle_line/2, Acc, Device),
    Combinations = fill(?RUCKSACK, Containers),
    case ?PART of
        part1 -> length(Combinations);
        part2 ->
            Foo = lists:sort(lists:map(fun erlang:length/1, Combinations)),
            Shortest = erlang:hd(Foo),
            length(lists:takewhile(fun(L) -> L =:= Shortest end, Foo))
    end.

handle_line(Line, Acc) ->
    [list_to_integer(Line)|Acc].

fill(Volume, Containers) ->
    %% fill(Volume, Containers, []).
    lists:filter(fun(CList) -> lists:sum(CList) =:= Volume end, power_set(Containers)).

fill(Volume, _, Acc) when Volume =:= 0 ->
    io:format("HIT! -> ~p ~n", [Acc]),
    [Acc];

fill(Volume, _, _) when Volume < 0 -> 
    [];

fill(Volume, Containers, Acc) ->
    io:format("~n~p, ~p, ~p~n", [Volume, Containers, Acc]),
    ValidContainers = lists:filter(fun(C) -> C =< Volume end, Containers),
    Combinations = lists:map(
        fun(Index) ->
            Container = lists:nth(Index, Containers),
            Remaining = lists:sublist(Containers, 1, Index-1) ++ lists:sublist(Containers, Index+1, length(Containers)),
            fill(Volume-Container, Remaining, [Container|Acc])
        end, lists:seq(1, length(ValidContainers))),
    %% from list of lists of lists to list of lists
    C2 = lists:append(Combinations),
    %% remove empty lists
    io:format("-> ~p~n", [C2]),
    lists:filter(fun(L) -> L =/= [] end, C2).


power_set([]) ->
    [[]];
power_set([H|T]) ->
    FirstHalf = power_set(T),
    %% SecondHalf = lists:map(fun(L) -> L ++ [H] end, FirstHalf),
    SecondHalf = [fun(L) -> L ++ [H] end || L <- FirstHalf],
    FirstHalf ++ SecondHalf.
