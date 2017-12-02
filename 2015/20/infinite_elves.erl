-module(infinite_elves).
-compile(export_all).

-define(PART, part2).

start() ->
    Device = toolbox:get_device({file, "input"}),
    Input = list_to_integer(io:get_line(Device, "") -- "\n"),
    Houses = lists:seq(1, lists:max(factors(Input) -- [Input]) div 10),
    XS = lists:dropwhile(
        fun(House) ->
            P = presents(House),
            io:format("House ~p gets ~p presents~n", [House, P]),
            P < Input
        end, Houses),
    erlang:hd(XS).

presents(House) ->
    case ?PART of
        part1 ->
            10 * lists:sum(factors(House));
        part2 ->
            VisitedElves = lists:dropwhile(
                fun(Factor) ->
                    House div Factor > 50
                end, factors(House)),
            11 * lists:sum(VisitedElves)
    end.

factors(1) -> [1];
factors(N) ->
    lists:sort(factors(N, 2, math:sqrt(N), [1,N])).

factors(_, K, Q, Acc) when K > Q ->
    Acc;

factors(N, K, Q, Acc) when K * K == N ->
    factors(N, K+1, Q, [K|Acc]);

factors(N, K, Q, Acc) when N rem K == 0 ->
    factors(N, K+1, Q, [K, N div K|Acc]);

factors(N, K, Q, Acc) ->
    factors(N, K+1, Q, Acc).

