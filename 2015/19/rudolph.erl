-module(rudolph).
-compile(export_all).

-type token() :: {noise, string()}
               | {token, string()}
               .
-type token_string() :: [token()].
-type rules() :: [{string(), [string()]}].

-define(PART, test).


main() ->
    Device = toolbox:get_device({file, "input"}),
    Acc = {orddict:new(), ""},
    {Rules, Target} = toolbox:fold_lines(fun handle_line/2, Acc, Device),
    TokenString = tokenize(Target, orddict:fetch_keys(Rules)),
    case ?PART of
        part1 -> length(replace_one(TokenString, Rules));
        part2 -> ok;
        test ->
            io:format("Rules: ~p~n", [Rules]),
            io:format("TokenString: ~p~n", [TokenString]),
            RS = lists:sort(lists:append([T || {S, T} <- Rules])),
            RS2 = lists:sort([lists:reverse(R) || R <- RS]),
            io:format("RS: ~p~n", [RS2])
    end.

-spec reverse_rules(rules()) -> list({string(), string()}).
reverse_rules(Rules) -> 
    lists:foldl(
        fun({Source, Targets}, Acc) ->
            lists:foldl(
                fun(T, Acc2) ->
                    RevT = lists:reverse(T),
                    RevSource = lists:reverse(Source),
                    case lists:keyfind(RevT, 1, Acc2) of 
                        {RevT, _} -> erlang:error(not_suffix_free);
                        false -> lists:keystore(RevT, 1, Acc2, {RevT, RevSource})
                    end,
                end, Acc, Targets)
        end, [], Rules).

-spec reverse_token_string(token_string()) -> string().
reverse_token_string(TokenString) -> 
    lists:reverse(lists:flatten([Token || {Type, Token} <- TokenString])).

-spec handle_line(string(), {rules(), string()}) -> {rules(), string()}.
handle_line(Line, {Rules, Target}) ->
    Regex = "^(.*?) => (.*)$",
    Match = toolbox:match_line(Line, Regex),
    case Match of
        [Src, Dst] -> {orddict:append(Src, Dst, Rules), Target};
        nomatch -> {Rules, Line}
    end.

-spec replace_one(token_string(), rules()) -> token_string().
replace_one(TokenString, Rules) ->
    replace_one(TokenString, Rules, sets:new(), "").

replace_one([], Rules, RAcc, SAcc) ->
    sets:to_list(RAcc);

replace_one([H|TokenString], Rules, RAcc, SAcc) ->
    case H of
        {token, T} ->
            RemainingString = lists:flatten(lists:map(fun({_,S}) -> S end, TokenString)),
            Reps = orddict:fetch(T, Rules),
            Replaced = lists:map(
                fun(R) ->
                    SAcc ++ R ++ RemainingString
                end, Reps),
            replace_one(TokenString, Rules, sets:union(sets:from_list(Replaced), RAcc), SAcc ++ T);
        {noise, N} ->
            replace_one(TokenString, Rules, RAcc, SAcc ++ N)
    end.

-spec tokenize(string(), list()) -> token_string().
tokenize(String, Tokens) ->
    lists:filter(
        fun({A, Token}) ->
            %% we don't want to see empty noise
            A =/= noise orelse Token =/= []
        end, tokenize(String, Tokens, [], [])).

-spec tokenize(string(), list(), list(list()), list()) -> token_string().
tokenize([], _, TokenAcc, NoiseAcc) ->
    lists:reverse([{noise, lists:reverse(NoiseAcc)}|TokenAcc]);

tokenize(String, Tokens, TokenAcc, NoiseAcc) ->
    First = lists:foldl(
        fun(T, Found) ->
            case Found of
                not_found ->
                    case lists:prefix(T, String) of
                        true -> {token, T};
                        false -> not_found
                    end;
                Found -> Found
            end
        end, not_found, Tokens),
    case First of
        {token, T} ->
            tokenize(String -- T, Tokens, [{token, T}, {noise, lists:reverse(NoiseAcc)}|TokenAcc], "");
        not_found ->
            [H|Rest] = String,
            tokenize(Rest, Tokens, TokenAcc, [H|NoiseAcc])
    end.
