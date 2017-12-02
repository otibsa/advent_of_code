-module(reindeer).
-compile(export_all).

-define(PART, part2).

start() ->
    Device = toolbox:get_device({file, "input"}),
    Acc = [],
    Reindeers = toolbox:fold_lines(fun handle_line/2, Acc, Device),
    T = 2503,
    case ?PART of
        part1 -> longest_distance(T, Reindeers);
        part2 -> most_points(T, Reindeers)
    end.

handle_line(Line, Acc) ->
    Regex = "^([[:alpha:]]+) can fly ([[:digit:]]+) km/s for ([[:digit:]]+) seconds, but then must rest for ([[:digit:]]+) seconds.$",
    [Name, SpeedS, DurationS, RestS] = toolbox:match_line(Line, Regex),
    Speed = list_to_integer(SpeedS),
    Duration = list_to_integer(DurationS),
    Rest = list_to_integer(RestS),
    lists:keystore(Name, 1, Acc, {Name, Speed, Duration, Rest}).

longest_distance(T, Reindeers) ->
    lists:foldl(
      fun(Reindeer={Name, _, _, _}, {AccN, AccD}) -> 
              D = distance_traveled(T, Reindeer),
              if D > AccD -> {Name, D}
               ; D =< AccD -> {AccN, AccD}
              end
      end, {nobody, -42}, Reindeers).

most_points(T, Reindeers) ->
    %% fold TNow through [1,...,T], Acc = [{{Name, Speed, Duration, Rest},Distance,Points}, ...]
    %% at each iteration, compare reindeers, give points
    %%    => All = map {A,D,P} -> {A,distance_traveled(TNow, R),P} through Acc
    %%       MaxD = fold (max D) All
    %%       map (if D == MaxD then P++) through All 
    BoardInit = lists:map(fun(R) -> {R, 0, 0} end, Reindeers),
    Final = lists:foldl(
        fun(TNow, Board) ->
            Board2 = lists:map(fun({R,D,P}) -> {R, distance_traveled(TNow, R), P} end, Board),
            MaxD = lists:foldl(
                fun({R,D,P}, Acc) ->
                    if D > Acc -> D;
                       D =< Acc -> Acc
                    end
                end, -42, Board2),
            lists:map(
              fun({R,D,P}) -> 
                  if D =:= MaxD -> {R,D,P+1}
                   ; D =/= MaxD -> {R,D,P}
                  end
              end, Board2)
        end, BoardInit, lists:seq(1,T)),
    lists:foldl(
        fun({R,D,P}, {AccN, AccD, AccP}) ->
            if P > AccP -> {R,D,P}
             ; P =< AccP -> {AccN, AccD, AccP}
            end
        end, {nobody, -42, -1337}, Final).

distance_traveled(T,Reindeer) ->
    distance_traveled(T, Reindeer, 0).

distance_traveled(T, Reindeer={Name, Speed, Duration, Rest}, Acc) when T >= 0 ->
    case T < Duration of
        true -> Speed * T + Acc;
        false -> case T < Duration+Rest of
                     true -> Speed * Duration + Acc;
                     false -> distance_traveled(T-Duration-Rest, Reindeer, Acc+Speed*Duration)
                 end
    end.
