-module(houses).
-compile(export_all).

start() ->
    Device = get_device(stdin),
    {_, VisitedHouses} = fold_character(Device, fun(Acc, Data) -> go_step(Acc, Data) end, {{0,0}, [{0,0}]}),
    io:format("Number of visited houses: ~p~n", [length(VisitedHouses)]),
    %% io:format("Visited houses: ~p~n", [VisitedHouses]),
    halt().

go_step({{X,Y}, VisitedHouses}, Direction) ->
    Pos = case Direction of
              $< -> {X-1, Y};
              $> -> {X+1, Y};
              $^ -> {X, Y+1};
              $v -> {X, Y-1};
              _ -> stationary 
          end,
    case lists:member(Pos, VisitedHouses) of
        false when Pos =/= stationary ->
            {Pos, [Pos|VisitedHouses]};
        _ -> 
            {Pos, VisitedHouses}
    end.


    
fold_character(Device, Function, Acc) ->
    case file:read(Device, 1) of
        eof ->
            file:close(Device),
            Acc;
        {ok, [Data]} ->
            fold_character(Device, Function, Function(Acc, Data))
    end.


get_device(Source) -> 
    {ok, Device} =  case Source of
        stdin -> {ok, standard_io};
        {file, Filename} -> file:open(Filename, [read])
    end,
    Device.
