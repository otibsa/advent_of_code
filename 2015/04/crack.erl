-module(crack).
-compile(export_all).

start() ->
    start(7),
    halt().

start(Count) ->
    Input = "bgvyzdsv",
    {Hash, Number} = crack(Input, 1, Count),
    io:format("Hash = ~p~nNumber = ~p~nInput = ~p~n", [bin_to_hex_string(Hash), Number, Input++integer_to_list(Number)]),
    halt().

crack(Input, Number, Count) ->
    Hash = erlang:md5(Input ++ integer_to_list(Number)),
    case starts_with_zeros(Hash, Count) of
        true -> 
            {Hash, Number};
        false -> 
            crack(Input, Number+1, Count)
    end.

starts_with_zeros(_, 0) ->
    true;
starts_with_zeros(Bin, Count) ->
    case Bin of
        <<0:4, Rest/bitstring>> -> starts_with_zeros(Rest, Count-1);
        _ -> false
    end.

    
bin_to_hex_string(Bin) -> 
    lists:flatten([io_lib:format("~2.16.0b", [X]) || X<- binary_to_list(Bin)]).

