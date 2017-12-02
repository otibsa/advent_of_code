-module(password).
-compile(export_all).

-define(PART, part2).
-define(PASS_LENGTH, 8).
-define(BASE_CHAR, $a).
-define(CARDINALITY, 26).


start() ->
    Input = case ?PART of
                part1 -> "vzbxkghb";
                part2 -> next_password("vzbxxyzz")
            end,
    toolbox:converge(fun next_password/1, fun valid/1, Input).

next_password(P) ->
    V = decode(offset(P, -1 * ?BASE_CHAR), ?CARDINALITY),
    offset(fix_length(encode(V+1, ?CARDINALITY), ?PASS_LENGTH), ?BASE_CHAR).

valid(P) ->
    %% contain c, c+1, c+2
    %% not contain i, o or l
    %% contain two distinct pairs nn, mm
    valid(P, 1, []).

valid([], SeqNeeded, PairsFound) -> 
    SeqNeeded =< 0 andalso length(PairsFound) >= 2;

valid("i"++_, _, _) -> false;
valid("o"++_, _, _) -> false;
valid("l"++_, _, _) -> false;

valid([C1], 0, PairsFound) ->
    valid([], 0, PairsFound);

valid([C1,C2], 0, PairsFound) ->
    %% io:format("looking at ~p, need seq: ~p, found pairs: ~p~n", [[C1,C2],0,PairsFound]),
    NewPair = case lists:member(C1, PairsFound) of
                  false when C1 =:= C2 ->
                      [C1];
                  _ -> 
                      []
              end,
    valid([C2], 0, NewPair ++ PairsFound);

valid([C1,C2,C3| T], SeqNeeded, PairsFound) ->
    %% io:format("looking at ~p, need seq: ~p, found pairs: ~p~n", [[C1,C2,C3],SeqNeeded,PairsFound]),
    NewS = case C1+1 of
        C2 -> case C1+2 of
                  C3 when SeqNeeded > 0 -> SeqNeeded - 1;
                  _ -> SeqNeeded
              end;
        _ -> 
            SeqNeeded
    end,
    NewPair = case lists:member(C1, PairsFound) of
                  false when C1 =:= C2 ->
                      [C1];
                  _ ->
                      []
              end,
    valid([C2,C3|T], NewS, NewPair ++ PairsFound);

valid(_,_,_) ->
    false.




offset(L, Zero) ->
    lists:map(fun(Digit) -> Digit+Zero end, L).

fix_length(L, Length) ->
    case length(L) < Length of
        true -> lists:duplicate(Length - length(L), 0) ++ L;
        false -> lists:reverse(lists:sublist(lists:reverse(L), Length))
    end.

%% 0,_ -> [0]
%% 15,16 -> [15]
%% 16,16 -> [1,0]
%% 255, 16 -> [15,15]
encode(V, Cardinality) ->
    encode(V, Cardinality, []).

encode(V, Cardinality, Acc) ->
    if V < Cardinality ->
           [V|Acc]
     ; V >= Cardinality ->
           encode(V div Cardinality, Cardinality, [V rem Cardinality|Acc])
    end.

decode(L, Cardinality) ->
    lists:foldl(fun(Digit, Acc) -> Acc*Cardinality + Digit end, 0, L).



