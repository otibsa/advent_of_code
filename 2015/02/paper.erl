-module(paper).
-compile(export_all).

start() ->
    %%io:format("foobar: ~n~p~n", [read_lines(stdin)]).
    io:format("~p~n", [parse_ribbon(stdin)]),
    halt().

read_lines(Source) ->
    %% 23ms
    %% fold_lines(Device, fun(Acc, Line) -> Acc ++ [Line] end, []).
    %% 13ms
    Device = get_device(Source),
    lists:reverse(fold_lines(Device, fun(Acc, Line) -> [Line|Acc] end, [])).

parse_ribbon(Source) ->
    Device = get_device(Source),
    fold_lines(Device, fun(Acc, Line) -> Acc + calculate_ribbon(lwh(Line)) end, 0).

calculate_ribbon({L,W,H}) ->
    2*min(L+W, min(W+H, L+H)) + L*W*H.


parse_paper(Source) ->
    Device = get_device(Source),
    fold_lines(Device, fun(Acc, Line) -> Acc + calculate_paper(lwh(Line)) end, 0).


%% wrap a box in paper, the smallest side gets two sheets
calculate_paper({L,W,H}) ->
    2*L*W + 2*W*H + 2*L*H + min(L*W, min(W*H, L*H)).

%% Apply a function to each line of the input. Accumulate the result
%% this is a foldl
fold_lines(Device, Function, Acc) ->
    case io:get_line(Device, "") of
        eof ->
            case Device of 
                standard_io -> ok;
                _ -> file:close(Device)
            end,
            Acc;
        Data ->
            Line = string:strip(Data, right, $\n),
            fold_lines(Device, Function, Function(Acc, Line))
    end.


%% small helper function
get_device(Source) -> 
    {ok, Device} =  case Source of
        stdin -> {ok, standard_io};
        {file, Filename} -> file:open(Filename, [read])
    end,
    Device.

lwh(Line) ->
    case string:tokens(Line, "x") of
        [L,W,H] -> {list_to_integer(L), list_to_integer(W), list_to_integer(H)};
        _ -> {0,0,0}
    end.


%% ~13ms for file: input
%% read_lines_from_device(Device, Acc, by_lines) ->
%%     case io:get_line(Device, "") of
%%         eof -> 
%%             case Device of
%%                 standard_io -> ok;
%%                 _ -> file:close(Device)
%%             end,
%%             lists:reverse(Acc);
%%         Data ->
%%             %% remove trailing newline
%%             Line = string:strip(Data, right, $\n),
%%             read_lines_from_device(Device, [Line|Acc], by_lines)
%%     end;

%% Acc contains only one line always
%% Only half the way to tail recursion, 
%% we create one stack frame for each line of the file
%%
%% ~44ms for file: input
%% read_lines_from_device(Device, Acc, by_chars) ->
%%     case file:read(Device, 1) of
%%         eof ->
%%             lists:reverse(Acc);
%%         {ok, [Data]} ->
%%             case Data of
%%                 $\n -> 
%%                     [lists:reverse(Acc) | read_lines_from_device(Device, [], by_chars)];
%%                 Data ->
%%                     read_lines_from_device(Device, [Data|Acc], by_chars)
%%             end
%%     end;
%% 
%% 
%% %% ~44ms for file: input
%% read_lines_from_device(Device, [], by_chars_tail) ->
%%     f(Device, [], []).
%% 
%% 
%% f(Device, LinesAcc, CharsAcc) ->
%%     case file:read(Device, 1) of
%%         eof ->
%%             lists:reverse(LinesAcc);
%%         {ok, [Data]} -> 
%%             case Data of
%%                 $\n ->
%%                     f(Device, [lists:reverse(CharsAcc)|LinesAcc], []);
%%                 Data ->
%%                     f(Device, LinesAcc, [Data|CharsAcc])
%%             end
%%     end.



%% read_lines(Filename) ->
%%     {ok, Bin} = file:read_file(Filename),
%%     string_to_lines(binary_to_list(Bin), []).
%% 
%% string_to_lines([], Acc) -> lists:reverse(Acc);
%% string_to_lines("\n" ++ String, Acc) -> [lists:reverse(Acc) | string_to_lines(String, [])];
%% string_to_lines([H|T], Acc) -> string_to_lines(T, [H|Acc]).
