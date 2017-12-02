-module(dinner).
-compile(export_all).

-define(PART, part2).

start() ->
    Device = toolbox:get_device({file, "input"}),
    Acc = [],
    find_seating(toolbox:fold_lines(fun handle_line/2, Acc, Device)).

find_seating(M) ->
    %%Seatings = lists:map(fun({From, _}) -> S = sit_down(From, [], M), {S, happiness(S)} end, M),
    %%{Seating, Happiness} = lists:foldl(fun happier_seating/2, {[], -999999}, Seatings).
    People = case ?PART of 
                 part1 -> M;
                 part2 -> add_person("Me", M)
             end,
    [{First, _}|_] = People, 
    Seating = sit_down(First, [], People),
    {Seating, happiness(Seating, People)}.

add_person(Person, M) ->
    lists:foldl(
      fun(N, AccM) ->
              MTmp = toolbox:matrix_store(Person, N, 0, AccM),
              toolbox:matrix_store(N, Person, 0, MTmp)
      end, M, lists:map(fun(X) -> element(1,X) end, M)).

%% return [A,B,D,...]
%%sit_down(Person, {SeatingAcc, HappinessAcc}, M) ->
sit_down(Person, SeatingAcc, M) ->
    NextSeating = [Person|SeatingAcc],
    Neighbors = lists:filter(
        fun({N,_}) ->
            not lists:member(N, NextSeating)
        end, toolbox:matrix_get_values(Person, M)),
    %% io:format("~p sits down, Seatings so far: ~p, Neighbors: ~p~n", [Person, SeatingAcc, Neighbors]),
    Seatings = lists:map(
        fun({N,_}) ->
                %%{Person, N_Person_Value} = toolbox:matrix_get(N, Person, M),
                %%io:format("maybe ~p next? ~n", [N]),
                %% NextHappiness = HappinessAcc + Person_N_Value + N_Person_Value,
                %%NextM = toolbox:matrix_delete(Person, M),
                %% io:format("next: ~p ~n", [NextSeating]),
                S = sit_down(N, NextSeating, M),
                %%io:format("At ~p: the best seating starting with ~p is ~p~n",[Person, N, S]),
                S
        end, Neighbors),
    %%io:format("Seatings for ~p = ~p~n", [Person, Seatings]),
    HS = lists:map(fun(S) -> happiness(S,M) end, Seatings),
    {BestS, BestH} = lists:foldl(fun happier_seating/2, {[], 0}, lists:zip(Seatings, HS)),
    case Neighbors of
        [] -> NextSeating;
        _ -> BestS
    end.


best_seating({From, _}, Acc, M) ->
    S = sit_down(From, [], M),
    happier_seating({S, happiness(S,M)}, Acc).


handle_line(Line, Acc) ->
    Regex = "^([[:alpha:]]+) would (gain|lose) ([[:digit:]]+) happiness units by sitting next to ([[:alpha:]]+)\\.$",
    [Person, Op, Amount, Neighbor] = toolbox:match_line(Line, Regex),
    %% store information in matrix
    Value = case Op of
                "gain" -> list_to_integer(Amount);
                "lose" -> -1 * list_to_integer(Amount)
            end,
    toolbox:matrix_store(Person, Neighbor, Value, Acc).

%% Seating: [A,B,C,D] 
happiness([], _ ) -> 0;
happiness([_|[]], _) -> 0;
happiness(Seating, M) -> 
    %% io:format("#### ~p~n", [M]),
    [H|T] = Seating,
    Zipped = lists:zip(Seating, T++[H]),
    lists:foldl(
      fun({From,To}, Acc) ->
              %% io:format("From ~p to ~p~n", [From, To]),
              case toolbox:matrix_get(From, To, M) of
                  {To, V1} -> case toolbox:matrix_get(To, From, M) of
                                  {From, V2} -> V1 + V2 + Acc;
                                  _ -> io:format("not found ~p in ~p~n",[{To, From}, M]),
                                       0
                              end;
                  _ -> 
                      io:format("not found ~p in ~p~n",[{To, From}, M]),
                      0
              end
      end, 0, Zipped).

%% max function on second element of a tuple
happier_seating({S,H}, {AccS, AccH}) ->
    if H > AccH -> 
           {S,H}
     ; H =< AccH ->
           {AccS, AccH}
    end.
