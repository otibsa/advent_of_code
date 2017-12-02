-module(houses2).
%% -export([main/0]).
-compile(export_all).

main() ->
    In = io:get_line("") -- "\n",
    Houses = dict:store({0,0}, 2, dict:new()),
    {Duration, Visited} = timer:tc(?MODULE, process_input, [In, Houses, {0,0}, {0,0}, 0]),
    io:format("~p us~nVisitedHouses: ~p~n", [Duration, Visited]),
    halt().

process_input([], Houses, _, _, _) ->
    dict:size(Houses);
process_input([H|T], Houses, {X,Y}, RXY, 0) ->
    process_input(T, deliver_gift(H, Houses, X, Y), update_loc(H, X, Y), RXY, 1);
process_input([H|T], Houses, SXY, {X, Y}, 1) ->
    process_input(T, deliver_gift(H, Houses, X, Y), SXY, update_loc(H, X, Y), 0).

deliver_gift(Dir, Houses, X, Y) ->
    case Dir of
        $^ -> dict:update_counter({X,Y+1}, 1, Houses);
        $> -> dict:update_counter({X+1,Y}, 1, Houses);
        $v -> dict:update_counter({X,Y-1}, 1, Houses);
        $< -> dict:update_counter({X-1,Y}, 1, Houses)
    end.

update_loc($^, X, Y) -> {X,Y+1};
update_loc($>, X, Y) -> {X+1,Y};
update_loc($v, X, Y) -> {X,Y-1};
update_loc($<, X, Y) -> {X-1,Y}.
