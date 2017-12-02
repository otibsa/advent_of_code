-module(game_of_lights).
-compile(export_all).

-define(PART, part2).
-define(DX, 100).
-define(DY, 100).
-define(STEPS, 100).


main() ->
    Device = toolbox:get_device({file, "input"}),
    Acc = {0, new_2d_array(?DX, ?DY)},
    {_, Lights} = toolbox:fold_lines(fun handle_line/2, Acc, Device),
    count_lights(simulate(Lights, ?STEPS)).

handle_line(Line, {Index, Arr}) ->
    LightRow = lists:map(
        fun(C) ->
            case C of
                $# -> on;
                $. -> off
            end
        end, Line),
    {Index+1, array:set(Index, array:from_list(LightRow, off), Arr)}.

simulate(Lights, Steps) ->
    lists:foldl(fun(_, Acc) -> simulate_step(Acc) end, Lights, lists:seq(1,Steps)).

simulate_step(Lights) ->
    %% for each Light:
    %%     Neighbors = 
    %%     apply_rules(Light, Neighbors)
    array:map(
        fun(Y, Row) ->
            array:map(
                fun(X, Light) -> 
                    apply_rules(X,Y, Light, Lights)
                end, Row)
        end, Lights).

apply_rules(X,Y,Light,Lights) ->
    case ?PART of
        part1 ->
            apply_rules_real(X,Y,Light,Lights);
        part2 ->
            if X =:= 0, Y =:= 0 -> on
             ; X =:= ?DX-1, Y =:= 0 -> on
             ; X =:= 0, Y =:= ?DY-1 -> on
             ; X =:= ?DX-1, Y =:= ?DY-1 -> on
             ; true -> apply_rules_real(X,Y,Light,Lights)
            end
    end.

apply_rules_real(X, Y, Light, Lights) ->
    Neighbors = count_neighbors(X,Y, Lights),
    case Light of
        on when Neighbors =:= 2; Neighbors =:= 3 ->
            on;
        on ->
            off;
        off when Neighbors =:= 3 ->
            on;
        off ->
            off
    end.

count_neighbors(X, Y, Lights) ->
    Neighbors = [{X-1,Y-1}, {X,Y-1}, {X+1, Y-1},
                 {X-1, Y} ,          {X+1, Y}, 
                 {X-1, Y+1}, {X,Y+1}, {X+1, Y+1}],
    lists:foldl(
        fun({NX,NY}, Acc) ->
                case get_light(NX, NY, Lights) of
                    on -> Acc + 1;
                    off -> Acc
                end
        end, 0, Neighbors).

get_light(X,Y, _) when X < 0 ; Y < 0 ; X >= ?DX ; Y >= ?DY ->
    off;

get_light(X,Y, Lights) ->
    case ?PART of
        part1 ->
            Row = array:get(Y, Lights),
            array:get(X, Row);
        part2 ->
            if X =:= 0, Y =:= 0 -> on
             ; X =:= ?DX-1, Y =:= 0 -> on
             ; X =:= 0, Y =:= ?DY-1 -> on
             ; X =:= ?DX-1, Y =:= ?DY-1 -> on
             ; true ->
                   Row = array:get(Y, Lights),
                   array:get(X, Row)
            end
    end.

count_lights(Lights) ->
    array:foldl(
        fun(_, Row, RowAcc) ->
            array:foldl(
                fun(_, Light, Acc) -> 
                    case Light of
                        on -> Acc + 1;
                        off -> Acc
                    end
                end, RowAcc, Row)
        end, 0, Lights).

new_2d_array(DX, DY) -> 
    array:new(DX, {default, array:new(DY, {default, off})}).
