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

structure() ->
    %% list.
    %% tree.
    %% ordered_list_tail.
    %% ordered_list.
    dict.
    %% dict_2.


%% start(Actor) ->
%%     Device = get_device(stdin),
%%     Input = io:get_line(Device, "") -- "\n",
%%     StartPos = {0,0},
%%     Structure = insert_unique(StartPos, create()),
%%     Acc = {StartPos, Structure},
%%     %% {Duration, {_, VisitedHouses}} = timer:tc(?MODULE, fold_character, [Device, fun ?MODULE:go_step/2, Acc]),
%%     {Duration, {_, VisitedHouses}} = timer:tc(lists, foldl, [fun ?MODULE:go_step/2, Acc, Input]),
%%     io:format("~p us~n", [Duration]),
%%     io:format("Number of visited houses: ~p~n", [my_length(VisitedHouses)]),
%%     halt().

start() ->
    start(just_santa),
    halt().

start(Actor) ->
    Device = get_device(stdin),
    Input = io:get_line(Device, "") -- "\n",
    StartPos = {0,0},
    %% Structure = insert_unique(StartPos, create()),
    Acc = {Actor, StartPos, StartPos, dict:new()},
    %% {Duration,{_,_,_,VisitedHouses}} = timer:tc(?MODULE, fold_character, [Device, fun ?MODULE:go_step_2/2, Acc]),
    {Duration,{_,_,_,VisitedHouses}} = timer:tc(lists, foldl, [fun ?MODULE:go_step/2, Acc, Input]),
    io:format("~p us~n", [Duration]),
    io:format("Number of visited houses: ~p~n", [my_length(VisitedHouses)]).


my_length(Structure) ->
    case structure() of
        tree -> gb_trees:size(Structure);
        dict -> dict:size(Structure);
        dict_2 -> dict:size(Structure);
        _ -> length(Structure)
    end.

create() ->
    case structure() of
        tree -> gb_trees:empty();
        dict -> dict:new();
        dict_2 -> dict:new();
        _ -> []
    end.

%% this function returns true or false
contains(Structure, Element) ->
    case structure() of
        tree -> gb_trees:is_defined(Element, Structure);
        list -> lists:member(Element, Structure);
        dict -> dict:is_key(Element, Structure);
        _ -> undefined_contains
    end.

%% this function assumes, that Element is *not* yet in Structure
insert(Element, Structure) ->
    case structure() of
        list -> [Element|Structure];
        tree -> gb_trees:insert(Element, visited, Structure);
        dict -> dict:append(Element, visited, Structure);
        _ -> undefined_insert%% insert_unique(Element, Structure) %% could return somethin bad 
    end.

%% this function inserts Element only if it does not exist yet
insert_unique(Element, Structure) ->
    case structure() of
        ordered_list ->
            ordered_insert(Element, Structure);
        ordered_list_tail ->
            ordered_insert_tail(Element, Structure, []);
        dict_2 ->
            dict:store(Element, visited, Structure);
        _ ->
            case contains(Structure, Element) of
                true -> Structure;
                false -> insert(Element, Structure)
            end
    end.

%% only insert if unique!
ordered_insert(Element, []) ->
    [Element];
ordered_insert(Element, [H|T]) ->
    if Element > H ->
           [H|ordered_insert(Element, T)] %% this is bad
     ; Element < H ->
           [Element, H|T]
     ; Element =:= H ->
           [H|T] %% don't insert duplicates
    end.

ordered_insert_tail(Element, [], Acc) ->
    lists:reverse([Element|Acc]);
ordered_insert_tail(Element, [H|T], Acc) ->
    if Element > H ->
           ordered_insert_tail(Element, T, [H|Acc])
     ; Element < H ->
           lists:reverse([H, Element|Acc]) ++ T  %% this is bad!
     ; Element =:= H ->
           lists:reverse([H|Acc]) ++ T %% don't insert duplicates
    end.


%% go_step(Acc, Data)
%% go_step(Direction, {{X,Y}, VisitedHouses}) ->
%%     Pos = case Direction of
%%               $< -> {X-1, Y};
%%               $> -> {X+1, Y};
%%               $^ -> {X, Y+1};
%%               $v -> {X, Y-1};
%%               _ -> stationary 
%%           end,
%%     if Pos =/= stationary ->
%%            %% update
%%            {Pos, insert_unique(Pos, VisitedHouses)}
%%            %% case search(Pos, VisitedHouses) of
%%            %%     false -> {Pos, insert(Pos, VisitedHouses)};
%%            %%     true -> {Pos, VisitedHouses}
%%            %% end
%%            %% /update
%%      ; Pos =:= stationary ->
%%            {Pos, VisitedHouses}
%%     end.
%%     %%case search(Pos, VisitedHouses) of
%%     %%    false when Pos =/= stationary ->
%%     %%        {Pos, insert(Pos, VisitedHouses)};
%%     %%    _ -> 
%%     %%        {Pos, VisitedHouses}
%%     %%end.

update_pos($<, {X, Y}) -> {X-1, Y};
update_pos($>, {X, Y}) -> {X+1, Y};
update_pos($^, {X, Y}) -> {X, Y+1};
update_pos($v, {X, Y}) -> {X, Y-1}.

%% go_step_2(Acc, Data)
go_step(Direction, {just_santa, SantaPos, RoboPos, VisitedHouses}) ->
    NewSanta = update_pos(Direction, SantaPos),
    {just_santa, NewSanta, RoboPos, dict:update_counter(NewSanta, 1, VisitedHouses)};

go_step(Direction, {santa, SantaPos, RoboPos, VisitedHouses}) ->
    NewSanta = update_pos(Direction, SantaPos),
    {robo, NewSanta, RoboPos, dict:update_counter(NewSanta, 1, VisitedHouses)};

go_step(Direction, {robo, SantaPos, RoboPos, VisitedHouses}) ->
    NewRobo = update_pos(Direction, RoboPos),
    {santa, SantaPos, NewRobo, dict:update_counter(NewRobo, 1, VisitedHouses)}.


go_step_2(Direction, {Actor, SantaPos, RoboPos, VisitedHouses}) ->
    {X,Y} = case Actor of
                santa -> SantaPos;
                robo -> RoboPos
            end,
    Pos = case Direction of 
              $< -> {X-1, Y};
              $> -> {X+1, Y};
              $^ -> {X, Y+1};
              $v -> {X, Y-1};
              _ -> stationary 
          end,
    %% io:format("~p at ~p after '~p'~n", [Actor, Pos, Direction]),
    if Pos =/= stationary ->
           flip(Actor, Pos, SantaPos, RoboPos, insert_unique(Pos, VisitedHouses))
     ; Pos =:= stationary ->
           flip(Actor, Pos, SantaPos, RoboPos, VisitedHouses)
    end.
    %% case search(Pos, VisitedHouses) of
    %%     false when Pos =/= stationary ->
    %%         case Actor of
    %%             santa -> {robo, Pos, RoboPos, insert(Pos, VisitedHouses)};
    %%             robo -> {santa, SantaPos, Pos, insert(Pos, VisitedHouses)}
    %%         end;
    %%     _ -> 
    %%         case Actor of
    %%             santa -> {robo, Pos, RoboPos, VisitedHouses};
    %%             robo -> {santa, SantaPos, Pos, VisitedHouses}
    %%         end
    %% end.

flip(Actor, Pos, SantaPos, RoboPos, VisitedHouses) ->
    case Actor of
        santa -> {robo, Pos, RoboPos, VisitedHouses};
        robo -> {santa, SantaPos, Pos, VisitedHouses}
    end.

fold_character(Device, Function, Acc) ->
    case file:read(Device, 1) of
        eof ->
            file:close(Device),
            Acc;
        {ok, [Data]} ->
            fold_character(Device, Function, Function(Data, Acc))
    end.


get_device(Source) -> 
    {ok, Device} =  case Source of
        stdin -> {ok, standard_io};
        {file, Filename} -> file:open(Filename, [read])
    end,
    Device.

