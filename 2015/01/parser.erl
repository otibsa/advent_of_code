-module(parser).
-compile(export_all).


%% Santa is on the ground floor (floor 0) and follows instructions.
%% If the input is '(', Santa goes up one floor, if ')' he goes down.
%%
%% Given a string of those inputs, return the floor, Santa is on at the end.
%%
%% (()) -> 0
%% ((( -> 3

part_1() ->
    get_last_floor(io:get_line("Input> "), 0).

part_2() ->
    Result = first_negative(io:get_line(""), 0, 1),
    io:format("~p~n", [Result]).

-spec get_last_floor(list(), integer()) -> integer().
get_last_floor([], Floor) ->
    Floor;

get_last_floor([H|T], Floor) ->
    get_last_floor(T, parse_char(H, Floor)).

-spec first_negative(list(), integer(), integer()) -> integer().
first_negative([], _, _) ->
    error;

first_negative([H|T], Floor, Step) -> 
    NextFloor = parse_char(H, Floor),
    if NextFloor < 0 -> 
           Step;
       NextFloor >= 0 ->
           first_negative(T, NextFloor, Step+1)
    end.

-spec parse_char(char(), integer()) -> integer().
parse_char(Instruction, Floor) ->
    case Instruction of 
        $( -> Floor+1;
        $) -> Floor-1;
        _ -> Floor
    end.

