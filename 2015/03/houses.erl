-module(houses).
-compile(export_all).

%% 
%%                  start_1  start_2
%%       !fold_character!
%% list                 345      350  ms
%% gb_trees             345      348  ms
%% ordered_list_tail    614      557  ms
%% ordered_list         546      508  ms
%% dict                 348      344  ms
%% dict_2               356      357  ms
%%
%%       !foldl on get_line!
%% list                 305      347   ms
%% gb_trees               9.9      9.8 ms
%% ordered_list_tail    309      244   ms
%% ordered_list         242      183   ms
%% dict                   7.6      8.6 ms
%% dict_2                 8.6      9.4 ms
%%

start_1() ->
    start(part1),
    halt().

start_2() ->
    start(part2),
    halt().

start(Part) ->
    Device = get_device(stdin),
    Input = io:get_line(Device, "") -- "\n",
    StartPos = {0,0},
    VisitedHouses = dict:store(StartPos, 1, dict:new()),
    {Duration, Result} = case Part of
        part1 -> 
            %% just santa delivers
            timer:tc(?MODULE, santa_all, [Input, StartPos, VisitedHouses]);
        part2 -> 
            %% santa and robo deliver both
            timer:tc(?MODULE, both_all, [Input, StartPos, StartPos, VisitedHouses])
    end,
    %% fold is roughly 50 us (sic) faster than recursive
    {Duration_fold, _} = case Part of
        part1 ->
            timer:tc(lists, foldl, [fun ?MODULE:santa_step/2, {StartPos, VisitedHouses}, Input]);
        part2 ->
            timer:tc(lists, foldl, [fun ?MODULE:both_step/2, {StartPos, StartPos, VisitedHouses}, Input])
    end,

  %% {_,_,_,VisitedHouses} = lists:foldl(fun ?MODULE:go_step/2, Acc, Input),
    io:format("~p ~p", [Duration, Duration_fold]).

%% go_step_2(Data, Acc) -> NewAcc
santa_step(Direction, {Pos, VisitedHouses}) ->
    NewPos = update_pos(Direction, Pos),
    {NewPos, dict:update_counter(NewPos, 1, VisitedHouses)}.

both_step(Direction, {PosA, PosB, VisitedHouses}) ->
    NewPos = update_pos(Direction, PosA),
    {PosB, NewPos, dict:update_counter(NewPos, 1, VisitedHouses)}.


both_all([], _, _, VisitedHouses) ->
    dict:size(VisitedHouses);

both_all([H|T], PosA, PosB, VisitedHouses) ->
    %% update position of first guy (santa or robo)
    NewPos = update_pos(H, PosA),
    %% flip the two positions, recursion
    both_all(T, PosB, NewPos, dict:update_counter(NewPos, 1, VisitedHouses)).

santa_all([], _, VisitedHouses) ->
    dict:size(VisitedHouses);

santa_all([H|T], Pos, VisitedHouses) ->
    NewPos = update_pos(H, Pos),
    santa_all(T, NewPos, dict:update_counter(NewPos, 1, VisitedHouses)).


update_pos($<, {X, Y}) -> {X-1, Y};
update_pos($>, {X, Y}) -> {X+1, Y};
update_pos($^, {X, Y}) -> {X, Y+1};
update_pos($v, {X, Y}) -> {X, Y-1}.



get_device(Source) -> 
    {ok, Device} =  case Source of
        stdin -> {ok, standard_io};
        {file, Filename} -> file:open(Filename, [read])
    end,
    Device.

