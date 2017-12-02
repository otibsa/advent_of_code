-module(recipe).
-compile(export_all).

-define(PART, part1).
-define(SUM, 100).
-define(CALS, 500).

start() ->
    Device = toolbox:get_device({file, "input"}),
    Acc = [],
    Ingredients = toolbox:fold_lines(fun handle_line/2, Acc, Device),
    find_mix(Ingredients).

handle_line(Line, Acc) ->
    Regex = "^(.*?): capacity (.*?), durability (.*?), flavor (.*?), texture (.*?), calories (.*?)$",
    [Name|Prop] = toolbox:match_line(Line, Regex),
    [Capacity, Durability, Flavor, Texture, Calories] = lists:map(fun list_to_integer/1, Prop),
    lists:keystore(Name, 1, Acc, {Name, Capacity, Durability, Flavor, Texture, Calories}).

find_mix(Ingredients) ->
    find_mix(Ingredients, partitions(?SUM, length(Ingredients)), {none, -42}).

find_mix(Ingredients, [], Acc) ->
    Acc;
find_mix(Ingredients, [P|T], Acc) ->
    {BestPartition, BestScore} = Acc,
    Names = lists:map(fun({Name, _, _, _, _, _}) -> Name end, Ingredients),
    Parts = length(Names),
    Mix = lists:zip(Names, P),
    case calories(Mix, Ingredients) of
        ?CALS -> 
            Score = score(Mix, Ingredients),
            case Score > BestScore of
                true -> find_mix(Ingredients, T, {P, Score});
                false -> find_mix(Ingredients, T, Acc)
            end;
        _ -> find_mix(Ingredients, T, Acc)
    end.

%% Mix = [{"Butterscotch", 44}, {...},...]
calories(Mix, Ingredients) ->
    %% 6 => Calories
    score(Mix, Ingredients, [6]).

%% %% [{"Sprinkles", 42,1337,0,4,1}, {...},...]
%% find_mix(Ingredients) ->
%%     Names = lists:map(fun({Name, _, _, _, _, _}) -> Name end, Ingredients),
%%     Parts = length(Names),
%%     lists:foldl(
%%       fun(Partition, {BestPartition, BestScore}) -> 
%%               Score = score(lists:zip(Names, Partition), Ingredients), 
%%               if Score > BestScore -> {Partition, Score}
%%                ; Score =< BestScore -> {BestPartition, BestScore}
%%               end
%%       end, {none, -42}, partitions(?SUM, Parts)).

%% Mix = [{"Butterscotch",10}, {"Cinnamon",50}, ...]
score(Mix, Ingredients) -> 
    %% Properties: Capacity, Durability, Flavor, Texture
    score(Mix, Ingredients, [2,3,4,5]).

score(Mix, Ingredients, Indices) ->
    lists:foldl(
      fun(P, Score) -> 
              PropScore = lists:foldl(
                            fun({Name, Amount}, AccP) ->
                                    AccP + Amount*element(P, lists:keyfind(Name, 1, Ingredients))
                            end, 0,Mix),
              Score * force_positive(PropScore)
      end, 1, Indices).

    %% C = lists:foldl(
    %%       fun({Name, Amount}, Acc) ->
    %%               Acc + Amount*element(2, lists:keyfind(Name, 1, Ingredients))
    %%       end, 0,Mix),
    %% D = lists:foldl(
    %%       fun({Name, Amount}, Acc) ->
    %%               Acc + Amount*element(3, lists:keyfind(Name, 1, Ingredients))
    %%       end, 0,Mix),
    %% F = lists:foldl(
    %%       fun({Name, Amount}, Acc) ->
    %%               Acc + Amount*element(4, lists:keyfind(Name, 1, Ingredients))
    %%       end, 0,Mix),
    %% T = lists:foldl(
    %%       fun({Name, Amount}, Acc) ->
    %%               Acc + Amount*element(5, lists:keyfind(Name, 1, Ingredients))
    %%       end, 0,Mix),
    %% force_positive(C)*force_positive(D)*force_positive(F)*force_positive(T).

force_positive(X) ->
    if X >= 0 -> X
     ; X < 0 -> 0
    end.

partitions(N, 1) -> [[N]];
%%partitions(N, 2) ->
%%    lists:map(fun(K) -> [K|partitions(N-K, 1)] end, lists:seq(0,N));
partitions(N, Parts) ->
    L = lists:map(
          fun(K) ->
                  lists:map(
                    fun(L2) ->
                            [K|L2]
                    end, partitions(N-K, Parts-1))
          end, lists:seq(0,N)),
    lists:append(L).
    

