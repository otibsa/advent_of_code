-module(traveling_santa).
-compile(export_all).

part() -> part2.
lexer() -> "lexer".
parser() -> "parser".

start() ->
    Device = toolbox:get_device({file, "input"}),
    toolbox:prepare_parser(lexer(), parser()),
    Acc = [],
    D = toolbox:fold_lines(fun handle_line/2, Acc, Device),
    traveling_santa(D).

handle_line(Line, Acc) ->
    {From, To, Dist} = toolbox:parse_line(lexer(), parser(), Line),
    DestsF = case lists:keyfind(From, 1, Acc) of
                     {From, Destinations} -> Destinations;
                     _ -> []
             end,
    DestsT = case lists:keyfind(To, 1, Acc) of
                     {To, Dest} -> Dest;
                     _ -> []
             end,
    AccTmp = lists:keystore(From, 1, Acc, {From, [{To, Dist}|DestsF]}),
    lists:keystore(To, 1, AccTmp, {To, [{From, Dist}|DestsT]}).


%% D = [{A, [{B, 42}, {C, 10}]}, {B, [{A, 42}]}, ...]
traveling_santa(D) ->
    Routes = lists:map(fun({Place,_}) -> travel_from(Place, D, 0) end, D),
    case part() of 
        part1 -> shortest_route(Routes);
        part2 -> longest_route(Routes)
    end.

travel_from(Place, D, Acc) ->
    %%io:format("Traveling from ~p (Acc=~p, len(D)=~p)~n", [Place, Acc, length(D)]),
    Neighbors = case lists:keyfind(Place, 1, D) of
                    {Place, N} -> N;
                    _ -> []
                end,
    %% io:format("Neighbors: ~p~n",[Neighbors]),
    Routes = lists:map(
        fun({N,Dist}) ->
                travel_from(N, remove_place(Place, D), Acc+Dist)
        end, Neighbors),
    %% io:format("Routes: ~p~n", [Routes]),
    {Path, Cost} = case part() of
                       part1 -> shortest_route(Routes);
                       part2 -> longest_route(Routes)
                   end,
    %% io:format("Shortest route: ~p~n", [{Path, Cost}]),
    case Neighbors of
        [] -> {[Place], Acc};
        _ ->  {[Place|Path], Cost}
    end.

shortest_route(Routes) ->
    lists:foldl(
      fun(R, Acc) ->
              {RP,RC} = R,
              {AccP, AccC} = Acc,
              if RC < AccC -> R
               ; RC >= AccC -> Acc
              end
      end, {[], 9999999}, Routes).

longest_route(Routes) ->
    lists:foldl(
      fun(R, Acc) ->
              {RP,RC} = R,
              {AccP, AccC} = Acc,
              if RC > AccC -> R
               ; RC =< AccC -> Acc
              end
      end, {[], 0}, Routes).

remove_place(Place, D) ->
    FromGone = lists:keydelete(Place, 1, D),
    lists:map(fun({From, DList}) -> {From, lists:keydelete(Place, 1, DList)} end, FromGone).

distance(From, To, D) ->
    case lists:keyfind(From, 1, D) of
        {From, Dests} ->
            case lists:keyfind(To, 1, Dests) of
                {To, Distance} -> Distance;
                _ -> 99999999
            end;
        _ -> 99999999
    end.
