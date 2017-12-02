-module(nice).
-compile(export_all).

start() ->
    start(stdin),
    halt().

start(Source) ->
    Device = toolbox:get_device(Source),
    %% apply count_nice/2 to every line of the input, accumulate result
    {_, Result} = toolbox:fold_lines(fun ?MODULE:count_nice/2, {part2, 0}, Device),
    io:format("Result = ~p~n", [Result]).
    

%% fun(Line, Acc) -> NewAcc
count_nice(Line, {Part, Count}) ->
    case nice({Part, Line}) of
        nice -> {Part, Count + 1};
        {naughty, Reason} -> 
            io:format("~p: ~p~n", [Line, Reason]),
            {Part, Count}
    end.

nice({part1, Line}) ->
    nice_1(Line, 3, 1, none);

nice({part2, Line}) ->
    nice_2(Line, no_two_pair, no_sandwich).

nice_2([], MaybeTwo, MaybeSandwich) -> 
    if MaybeTwo =:= two_pair, MaybeSandwich =:= sandwich ->
           nice;
       true ->
           {naughty, {MaybeTwo, MaybeSandwich}}
    end;

nice_2([_|[]], MaybeTwo, MaybeSandwich) ->
    nice_2([], MaybeTwo, MaybeSandwich);

nice_2([_,_|[]], MaybeTwo, MaybeSandwich) ->
    nice_2([], MaybeTwo, MaybeSandwich);

%% now the line is at least three characters long
nice_2([C1,C2,C3|T], MaybeTwo, MaybeSandwich) ->
    NewSandwich = if C1 =:= C3 -> sandwich
                   ; C1 =/= C3 -> MaybeSandwich
                  end,
    NewTwo = case contains_pair([C3|T], C1, C2) of
                 true -> two_pair;
                 false -> MaybeTwo
             end,
    nice_2([C2,C3|T], NewTwo, NewSandwich).

%% this makes it at least O(n^2), I think
contains_pair([], _, _) -> false;
contains_pair([_|[]], _, _) -> false;
contains_pair([A,B|T], X,Y) ->
    if A =:= X, B =:= Y -> true
     ; true -> contains_pair([B|T], X,Y)
    end.


nice_1([], 0, 0, _) ->
    nice;

nice_1([], VowelsNeeded, DoubleNeeded, _) when VowelsNeeded > 0; DoubleNeeded > 0 ->
    naughty;

nice_1("ab"++_, _, _, _) ->
    naughty;
nice_1("cd"++_, _, _, _) ->
    naughty;
nice_1("pq"++_, _, _, _) ->
    naughty;
nice_1("xy"++_, _, _, _) ->
    naughty;

nice_1([H|T], VowelsNeeded, DoubleNeeded, Previous) ->
    NewDoubleNeeded = case H of
                          Previous when DoubleNeeded > 0 ->
                             DoubleNeeded - 1;
                          _ ->
                             DoubleNeeded
                      end,
    NewVowelsNeeded = case lists:member(H, "aeiou") of
                          true when VowelsNeeded > 0 ->
                              VowelsNeeded - 1;
                          _ ->
                              VowelsNeeded
                      end,
    nice_1(T, NewVowelsNeeded, NewDoubleNeeded, H).

