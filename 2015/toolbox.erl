-module(toolbox).
-compile(export_all).

%% Apply a function to each line of the input. Accumulate the result
%% this is a foldl
fold_lines(Function, Acc, Device) ->
    case io:get_line(Device, "") of
        eof ->
            case Device of 
                standard_io -> ok;
                _ -> file:close(Device)
            end,
            Acc;
        Data ->
            Line = string:strip(Data, right, $\n),
            fold_lines(Function, Function(Line, Acc), Device)
    end.


%% small helper function
get_device(Source) ->
    {ok, Device} =  case Source of
        stdin -> {ok, standard_io};
        {file, Filename} -> file:open(Filename, [read])
    end,
    Device.

%% function composition
%% compose = f . g
%%
%% call with (compose(fun ..., fun ...))(Args)
compose(F,G) ->
    fun(X) ->
            F(G(X))
    end.

%% apply a function to an argument until a tester returns true
%% F :: fun(X :: T) -> X :: T
%% Tester :: fun(X :: T) -> boolean()
%% X :: T
converge(F, Tester, X) ->
    case Tester(X) of
        true -> X;
        _ -> converge(F, Tester, F(X))
    end.


prepare_parser(Lexer, Parser) -> 
    leex:file(Lexer),
    compile:file(Lexer),

    yecc:file(Parser),
    compile:file(Parser),
    {list_to_atom(Lexer), list_to_atom(Parser)}.

parse_line(Lexer, Parser, Line) ->
    {ok, Tokens, _} = erlang:apply(list_to_atom(Lexer), string, [Line]),
    {ok, ParseTree} = erlang:apply(list_to_atom(Parser), parse, [Tokens]),

    ParseTree.

match_line(Line, Regex) ->
    case re:run(Line, Regex) of
        {match, [CompleteLine|Matches]} ->
            lists:map(fun({Start, Len}) -> string:substr(Line, Start+1, Len) end, Matches);
        Foo -> Foo
    end.


%% Group == 1 -> whole line
match_line(Line, Regex, Groups) ->
    case re:run(Line, Regex) of
        {match, MatchList} ->
            lists:map(fun(Group) -> get_group(Group, MatchList, Line) end, Groups);
        nomatch -> nomatch
    end.

get_group(Group, MatchList, Line) ->
    {Start, Length} = lists:nth(Group, MatchList),
    string:substr(Line, Start+1, Length).


keyprepend(Key, TupleList, Element) ->
    Values = case lists:keyfind(Key, 1, TupleList) of
                 {Key, V} when is_list(V) -> V;
                 _ -> [] %% this destroys tuplelists with nonlist values
             end,
    lists:keystore(Key, 1, TupleList, {Key, [Element|Values]}).

%% Matrix = [ {a, [{c, 5}]} , {b, [{a, 1}]} , {c, [{a,5}]} ]
%% not necessarily symmetrical!
%%
%% matrix_store overwrites existing entries
matrix_store(From, To, Value, M) ->
    ValuesFrom = case lists:keyfind(From, 1, M) of
                     {From, V} -> lists:keystore(To, 1, V, {To, Value});
                     _ -> [{To, Value}]
                 end,
    lists:keystore(From, 1, M, {From, ValuesFrom}).

matrix_new() ->
    [].

%% returns {To, Value} or false
matrix_get(From, To, M) -> 
    lists:keyfind(To, 1, matrix_get_values(From, M)).

matrix_get_values(From, M) ->
    case lists:keyfind(From, 1, M) of
        {From, V} -> V;
        _ -> []
    end.

matrix_delete(Entry, M) ->
    FromGone = lists:keydelete(Entry, 1, M),
    lists:map(fun({From, Values}) -> {From, lists:keydelete(Entry, 1, Values)} end, FromGone).
