-module(rpg).
-compile(export_all).

-define(PART, part1).


start() ->
    %% go shopping, for each valid combination of items, play the game
    Combos = [{W,A,R1,R2} ||
               W <- lists:filter(fun(T) -> element(1,T) =:= weapon end, shop()),
               A <- [{armor,no_armor,0,0,0}|lists:filter(fun(T) -> element(1,T) =:= armor end, shop())],
               R1 <- [{ring,no_ring1,0,0,0}|lists:filter(fun(T) -> element(1,T) =:= ring end, shop())],
               R2 <- [{ring,no_ring2,0,0,0}|lists:filter(fun(T) -> element(1,T) =:= ring end, shop())],
               R1 =/= R2
             ],
    GoodCombos = lists:filter(
        fun({W,A,R1,R2}) ->
            {D_player, A_player} = calc_stats(W, A, R1, R2),
            Winner = next_turn(player, {100, D_player, A_player}, boss_stats()),
            case ?PART of
                part1 -> Winner =:= player;
                part2 -> Winner =:= boss
            end
        end, Combos),
    %%io:format("GoodCombos: ~p~n", [GoodCombos]),
    %% cheapest winning game/most expensive losing game
    {ExtremeCost,CompareFun} = case ?PART of
                      part1 -> {infinity, fun erlang:'<'/2};     %% atoms are bigger than integers
                      part2 -> {0, fun erlang:'>'/2}
                  end,
    lists:foldl(
        fun(Items, {ExtremeCost, BestItems}) -> 
            Cost = calc_cost(Items),
            case CompareFun(Cost,ExtremeCost) of
                true ->
                    {Cost, Items};
                _ -> 
                    {ExtremeCost, BestItems}
            end
        end, {ExtremeCost, none}, GoodCombos).

start_shopping() -> 
    Weapons = lists:filter(fun(T) -> element(1,T) =:= weapon end, shop()),
    WinningCombinations = [],
    lists:foldl(
        fun(Weapon, Acc) ->
            Acc ++ try_weapon(Weapon, Acc)
        end, WinningCombinations, Weapons).

try_weapon(Weapon, WC_Acc) ->
    Armors = [no_armor|lists:filter(fun(T) -> element(1,T) =:= armor end, shop())],
    lists:foldl(
        fun(A, Acc) ->
            Acc ++ try_armor(A, Weapon, Acc)
        end, WC_Acc, Armors).

try_armor(Armor, Weapon, WC_Acc) ->
    Rings = [no_ring1|lists:filter(fun(T) -> element(1,T) =:= ring end, shop())],
    lists:foldl(
        fun(R, Acc) ->
            Acc ++ try_ring1(R, Weapon, Armor, Acc)
        end, WC_Acc, Rings).

try_ring1(Ring1, Weapon, Armor, WC_Acc) ->
    Rings = [no_ring2|lists:filter(fun(T) -> element(1,T) =:= ring end, shop())] -- [Ring1],
    lists:foldl(
        fun(R2, Acc) ->
            Acc ++ try_ring2(R2, Weapon, Armor, Ring1, Acc)
        end, WC_Acc, Rings).

try_ring2(Ring2, Weapon, Armor, Ring1, Acc) ->
    io:format("Items ~p ~n", [[Weapon, Armor, Ring1, Ring2]]),
    {D_player, A_player} = calc_stats(Weapon, Armor, Ring1, Ring2),
    case next_turn(player, {100, D_player, A_player}, boss_stats()) of
        player -> 
            io:format("won~n"),
            [[Weapon, Armor, Ring1, Ring2]|Acc];
        boss -> Acc
    end.

calc_stats(Weapon, Armor, Ring1, Ring2) ->
    {weapon, _, _, D_W, A_W} = Weapon,
    {armor,_,_,D_A, A_A} = Armor,
    %%case Armor of
    %%    no_armor -> {armor, foo, bar, 0,0};
    %%    Armor -> Armor
    %%end,
    {ring,_,_,D_R1, A_R1} = Ring1,
    %%case Ring1 of
    %%    no_ring1 -> {ring,foo,bar,0,0};
    %%    Ring1 -> Ring1
    %%end,
    {ring,_,_,D_R2, A_R2} = Ring2,
    %%case Ring2 of
    %%    no_ring2 -> {ring,foo,bar,0,0};
    %%    Ring2 -> Ring2
    %%end,
    {D_W+D_A+D_R1+D_R2, A_W+A_A+A_R1+A_R2}.

calc_cost(none) -> 0;

calc_cost({W,A,R1,R2}) -> 
    element(3,W)+element(3,A)+element(3,R1)+element(3,R2).


%% calc_cost(Items) ->
%%     lists:foldl(
%%         fun(Item, Acc) -> 
%%             Acc + element(3, Item)
%%         end, 0, Items).

next_turn(boss, _, {HP_boss, _, _}) when HP_boss =< 0 ->
    player;

next_turn(player, {HP_player, _, _}, _) when HP_player =< 0 ->
    boss;

next_turn(player, {HP_player, D_player, A_player}, {HP_boss, D_boss, A_boss}) ->
    Damage = lists:max([1, D_player-A_boss]),
    %% io:format("HP_player: ~p, HP_boss: ~p~n", [HP_player, HP_boss]),
    %% io:format("Player does ~p Damage ~n", [Damage]),
    next_turn(boss, {HP_player, D_player, A_player}, {HP_boss-Damage, D_boss, A_boss});

next_turn(boss, {HP_player, D_player, A_player}, {HP_boss, D_boss, A_boss}) ->
    Damage = lists:max([1, D_boss-A_player]),
    %% io:format("HP_player: ~p, HP_boss: ~p~n", [HP_player, HP_boss]),
    %% io:format("Boss does ~p Damage ~n", [Damage]),
    next_turn(player, {HP_player-Damage, D_player, A_player}, {HP_boss, D_boss, A_boss}).

boss_stats() ->
    %% hitpoints, damage, armor
    {103, 9, 2}.

shop() ->
    [
        %%              Cost, Damage, Armor
     	{weapon, "Dagger", 8, 4, 0},
		{weapon, "Shortsword", 10, 5, 0},
		{weapon, "Warhammer", 25, 6, 0},
		{weapon, "Longsword", 40, 7, 0},
		{weapon, "Greataxe", 74, 8, 0},
		{armor, "Leather", 13, 0, 1},
		{armor, "Chainmail", 31, 0, 2},
		{armor, "Splintmail", 53, 0, 3},
		{armor, "Bandedmail", 75, 0, 4},
		{armor, "Platemail", 102, 0, 5},
		{ring, "Damage +1", 25, 1, 0},
		{ring, "Damage +2", 50, 2, 0},
		{ring, "Damage +3", 100, 3, 0},
		{ring, "Defense +1", 20, 0, 1},
		{ring, "Defense +2", 40, 0, 2},
		{ring, "Defense +3", 80, 0, 3}
    ].
