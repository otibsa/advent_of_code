-module(wizard).
-compile(export_all).

-define(PART, part2).
-define(DEBUG, false).


start() ->
    {HP_player, Mana_player,Armor_player} = player_stats(),
    {HP_boss, _} = boss_stats(),
    next_turn(player, {HP_player, HP_boss, Mana_player, Armor_player, []}, [], 9999999).

player_stats() -> {50,500,0}.
boss_stats() -> {71,10}.

spells() -> 
    [{magic_missile, 53},
     {drain,73},
     {shield,113},
     {poison,173},
     {recharge,229}].

print_state(Agent, {HP_player, HP_boss, Mana_player, Armor_player, ActiveEffects}) ->
    io:format("[~6w] p: (~p, ~p, ~p), b: ~p, e: ~p ~n", [Agent, HP_player, Mana_player, Armor_player, HP_boss, ActiveEffects]).

next_turn(_, {HP_player,_,_,_,_}, _, _) when HP_player =< 0 -> {infinity, boss};
next_turn(_, {_,HP_boss,_,_,_}, CastedSpells, _) when HP_boss =< 0 -> {mana_cost(CastedSpells), CastedSpells};
next_turn(_, {_,_,Mana_player,_,_}, _, _) when Mana_player =< 0 -> {infinity, boss};

%% next_turn(_,_,CastedSpells, _) when length(CastedSpells) >= 30 -> {infinity, boss};

next_turn(Agent, State, CastedSpells, ManaLimit) ->
    case length(CastedSpells) < 3 of
        true when Agent =:= player -> io:format("~p, ManaLimit: ~p~n", [CastedSpells, ManaLimit]);
        _ -> ok
    end,
    case lists:suffix([{poison,173},{magic_missile,53},{recharge,229},{poison,173},{magic_missile,53}], CastedSpells) of
        true ->
            print_state(Agent, State),
            io:format("CastedSpells: ~p~n~n", [CastedSpells]);
        _ -> ok
    end,
    HP_player = case ?PART of
        part2 when Agent =:= player -> element(1,State) - 1;
        _ -> element(1,State)
    end,
    case HP_player =< 0 of 
        true -> {infinity, boss};
        false ->
            TmpState = setelement(1,State,HP_player),
            %% print_state(Agent, State),
            %% foldl through ActiveEffects and apply them.
            AffectedState = apply_all_effects(TmpState),
            {HP_player, HP_boss, Mana_player, Armor_player, ActiveEffects} = AffectedState,
            case Agent of
                _ when HP_player =< 0 -> {infinity, boss};  %% only reachable in part 2
                _ when HP_boss =< 0 -> {mana_cost(CastedSpells), CastedSpells};
                boss ->
                    {_, D_boss} = boss_stats(),
                    Damage = lists:max([1,D_boss-Armor_player]),
                    next_turn(player, {HP_player-Damage, HP_boss, Mana_player, Armor_player, ActiveEffects}, CastedSpells, ManaLimit);
                player ->
                    PossibleSpells = lists:filter(
                        fun({Spell,Cost}) ->
                            %% cost of spell must be less than mana and
                            %% can't cast spell that had an active effect.
                            (Cost =< Mana_player) and not(lists:keymember(Spell, 1, ActiveEffects))
                        end, spells()),

                    {XCost, XSeq, _} = lists:foldl(
                        fun(Spell, {BestCost, BestSeq, TmpLimit}) ->
                            NextCost = mana_cost([Spell|CastedSpells]),
                            case NextCost > TmpLimit of
                                true ->
                                    {BestCost, BestSeq, TmpLimit};
                                false ->
                                    {ThisCost, ThisSeq} = next_turn(boss, cast_spell(Spell, AffectedState), [Spell|CastedSpells], TmpLimit),
                                    case ThisCost < BestCost of
                                        true when ThisCost < TmpLimit -> {ThisCost, ThisSeq, ThisCost};  %% wooohooo
                                        true when ThisCost >= TmpLimit -> {ThisCost, ThisSeq, TmpLimit};  %% nice
                                        false -> {BestCost, BestSeq, TmpLimit}
                                    end
                            end
                            %%{ThisCost, ThisSeq} = next_turn(boss, cast_spell(Spell, AffectedState), [Spell|CastedSpells]),
                            %%case ThisCost < BestCost of
                            %%    true -> {ThisCost, ThisSeq};
                            %%    false -> {BestCost, BestSeq}
                            %%end
                        end, {infinity, bossState2, ManaLimit}, PossibleSpells),
                    {XCost, XSeq}
            end
    end.

simulate_game({HP_player, Mana_player, Armor_player}, {HP_boss, Dmg_boss}, Spells) ->
    lists:foldl(
        fun(Spell, State) ->
            io:format("~n"),
            print_state(player, State),
            State2 = one_turn(player, Spell, State),
            io:format("~n"),
            print_state(boss, State2),
            one_turn(boss, Dmg_boss, State2)
        end,{HP_player, HP_boss, Mana_player, Armor_player, []}, Spells).


one_turn(player, Spell, State) ->
    AffectedState = apply_all_effects(State),
    {HP_player, HP_boss, Mana_player, Armor_player, ActiveEffects} = AffectedState,
    io:format("player casts ~p~n",[Spell]),
    case HP_boss =< 0 of
        true -> AffectedState;
        false -> cast_spell(Spell, AffectedState)
    end;

one_turn(boss, Dmg_boss, State) ->
    AffectedState = apply_all_effects(State),
    {HP_player, HP_boss, Mana_player, Armor_player, ActiveEffects} = AffectedState,
    case HP_boss =< 0 of
        true -> AffectedState;
        false -> 
            Damage = lists:max([1,Dmg_boss-Armor_player]),
            {HP_player-Damage, HP_boss, Mana_player, Armor_player, ActiveEffects}
    end.



mana_cost(CastedSpells) ->
    lists:foldl(
        fun({_, Cost}, Acc) ->
            Acc + Cost
        end, 0, CastedSpells).

cast_spell(Spell, State) -> 
    {HP_player, HP_boss, Mana_player, Armor_player, ActiveEffects}=State,
    {Name,Cost} = Spell,
    case Name of
        magic_missile ->
            {HP_player, HP_boss-4, Mana_player-Cost, Armor_player, ActiveEffects};
        drain ->
            {HP_player+2, HP_boss-2, Mana_player-Cost, Armor_player, ActiveEffects};
        shield -> 
            {HP_player, HP_boss, Mana_player-Cost, Armor_player, [{Name,6}|ActiveEffects]};
        poison -> 
            {HP_player, HP_boss, Mana_player-Cost, Armor_player, [{Name,6}|ActiveEffects]};
        recharge -> 
            {HP_player, HP_boss, Mana_player-Cost, Armor_player, [{Name,5}|ActiveEffects]}
    end.


apply_all_effects({HP_player,HP_boss,Mana_player,Armor_player,ActiveEffects}) ->
    {_,_,InitialArmor} = player_stats(), % kind of hacky
    State = {HP_player,HP_boss,Mana_player,InitialArmor,ActiveEffects},
    apply_all_effects(ActiveEffects, State).

apply_all_effects([], State) -> State;
apply_all_effects([H|T], State) ->
    {E, Duration} = H,
    apply_all_effects(T, apply_effect(E, State)).


apply_effect(Effect, {HP_player, HP_boss, Mana_player, Armor_player, ActiveEffects}) ->
    %% assume that the effect is still active
    {_,_,InitialArmor} = player_stats(),
    {NewHP_player, NewHP_boss, NewMana_player, NewArmor_player} = case Effect of
        shield -> debug_print("Shield increases armor by 7; "),
                  {HP_player, HP_boss, Mana_player, InitialArmor+7};
        recharge -> debug_print("Recharge provides 101 mana; "),
                    {HP_player, HP_boss, Mana_player+101, Armor_player};
        poison -> debug_print("Poison deals 3 damage; "),
                  {HP_player, HP_boss-3, Mana_player, Armor_player}%%
        %%_ -> {HP_player, HP_boss, Mana_player, Armor_player}
    end,
    %% Decrease duration of this Effect in ActiveEffects. Remove if necessary
    NewActiveEffects = lists:filtermap(
        fun(AE) ->
            case AE of
                {Effect, 1} ->
                    debug_print("It wears off~n"),
                    false; %% TODO: check for +/- 1 error
                {Effect, N} -> 
                    debug_print("It's timer is now ~p~n",[N-1]),
                    {true, {Effect, N-1}};
                _ -> true
            end
        end, ActiveEffects),
    {NewHP_player, NewHP_boss, NewMana_player, NewArmor_player, NewActiveEffects}.

debug_print(X) ->
    debug_print(X,[]).

debug_print(X,Y) ->
    case ?DEBUG of
        true -> io:format(X,Y);
        _ -> ok
    end.
