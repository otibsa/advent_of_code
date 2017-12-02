-module(lights).
-compile(export_all).

start() ->
    start(stdin),
    halt().

part() -> part1_arr.

dimensions() -> {1000,1000}.

start(Source) ->
    {XDim, YDim} = dimensions(),
    Device = toolbox:get_device(Source),
    InitAcc = case part() of
              part1_int -> 0;
              _ -> array:new(XDim, {default, array:new(YDim, {default, 0})})
          end,
    Result = toolbox:fold_lines(fun(Line, Acc) -> set_lights(parse_line(Line), Acc) end, InitAcc, Device),
    io:format("~p~n", [count_lights(Result)]).

parse_line(Line) ->
    %% io:format("~p~n", [count_lights(Lights)]), 
    %%      |--------- 2 -----------| | 3 | | 4 |         | 5 | | 6 |
    Reg = "^(turn on|turn off|toggle) (.+?),(.+?) through (.+?),(.+?)$",
    case re:run(Line, Reg) of
        {match, MatchList} ->
            {StartAction, LengthAction} = lists:nth(2, MatchList),
            Action = case string:substr(Line, StartAction+1, LengthAction) of
                         "turn on" -> on;
                         "turn off" -> off;
                         "toggle" -> toggle
                     end,
            [XStart, YStart, XEnd, YEnd] = lists:map(
                fun({StartS, LengthS}) -> 
                    %% cut out a part of Line and interpret it as an integer
                    S = string:substr(Line, StartS+1, LengthS),
                    list_to_integer(S)
                end, lists:sublist(MatchList, 3, 4)),

            {Action, XStart, YStart, XEnd, YEnd};

        nomatch ->
            %% a faulty line should not affect the lights
            nomatch
    end.

%% fun(Args, Acc) -> NewAcc
set_lights(nomatch, Lights) -> Lights;

set_lights({Action, XStart, YStart, XEnd, YEnd}, Lights) when XStart =< XEnd, YStart =< YEnd, XStart >= 0, YStart >= 0 ->
    {XDim, YDim} = dimensions(),
    if XEnd < XDim, YEnd < YDim ->
        Rows = lists:seq(YStart, YEnd),
        lists:foldl(fun(YRow, LightAcc) -> apply_to_row(LightAcc, Action, YRow, XStart, XEnd) end, Lights, Rows)
     ; XEnd =< XDim; YEnd =< YDim ->
        Lights
    end;

set_lights(_, Lights) -> Lights.


apply_to_row(Lights, Action, YRow, XStart, XEnd) ->
    case part() of
        part1_int ->
            %% create string of 1 bits and shift them to the right position
            Mask = n_bits(XEnd - XStart + 1) bsl index_2d(XStart, YRow),
            case Action of
                on -> Lights bor Mask;
                off -> Lights band bnot Mask;
                toggle -> Lights bxor Mask
            end;
        _ -> 
            OldRow = array:get(YRow, Lights),
            NewRow = array:map(
                fun(IndX, LightValue) ->
                    if IndX >= XStart, IndX =< XEnd -> 
                        apply_to_bulb(LightValue, Action)
                     ; true -> 
                        LightValue
                    end
                end, OldRow),
            array:set(YRow, NewRow, Lights)
    end.

apply_to_bulb(LightValue, Action) ->
    %% apply rules to single light bulb
    case part() of
        part1_arr ->
            case Action of
                on -> 1;
                off -> 0;
                toggle -> if LightValue =:= 0 -> 1;
                             LightValue =:= 1 -> 0
                          end
            end;
        part2 ->
            case Action of
                on -> LightValue + 1;
                off when LightValue > 0 -> LightValue - 1;
                toggle -> LightValue + 2;
                _ -> LightValue
            end
    end.

%% 0b1111 = (1 << 4) - 1 = 15 = 2^4 -1
n_bits(N) ->
    (1 bsl N) - 1.

index_2d(X,Y) ->
    {Width, _} = dimensions(),
    Y*Width + X.


count_lights(Lights)->
    case part() of
        part1_int -> 
            count_lights(Lights, 0);
        _ -> 
            array:sparse_foldl(fun count_row/3, 0, Lights)
    end.

count_row(_, RowArray, AccRows) ->
    array:sparse_foldl(fun add/3, AccRows, RowArray).

add(_, Value, Acc) -> Value + Acc.

%% all lights off => Lights==0
count_lights(Lights, Acc) ->
    if Lights > 0 ->
           NewAcc = Acc + (Lights band 1),
           count_lights(Lights bsr 1, NewAcc)
     ; Lights =:= 0 ->
           Acc
    end.

