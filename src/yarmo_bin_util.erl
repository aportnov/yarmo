-module(yarmo_bin_util).
-author('author <alex.portnov@gmail.com>').

-export([bin_replace/3, bin_split/2, bin_find/2, bin_to_list/1, thing_to_bin/1, md5/2]).

bin_replace(Bin, From, To) -> 
    bin_replace(Bin, erlang:byte_size(From), Bin, From, To, 0). 

bin_replace(OrigBin, FromLen, Bin, From, To, Cnt) -> 
    case Bin of 
        <<From:FromLen/binary, Right/binary>> -> 
            OtherRepl = bin_replace(Right, From, To), 
            <<Left:Cnt/binary, _/binary>> = OrigBin, 
            <<Left/binary, To/binary, OtherRepl/binary>>; 
        <<_:8, Other/binary>> -> 
            bin_replace(OrigBin, FromLen, Other, From, To, Cnt + 1); 
        <<>> -> 
            OrigBin 
    end.

bin_find(Search, Binary) when size(Search) > 0 ->
    case size(Binary) - size(Search) of
        Last when Last < 0 ->
            partial_find(Search, Binary, 0, size(Binary));
        Last ->
            bin_find(Search, size(Search), Binary, 0, Last)
    end.

bin_find(Search, SearchSize, Binary, Index, Last) when Index =< Last->
    case Binary of
        <<_:Index/binary, Search:SearchSize/binary, _/binary>> ->
            {exact, Index};
        _ ->
            bin_find(Search, SearchSize, Binary, Index + 1, Last)
    end;

bin_find(Search, SearchSize, Binary, Index, Last) when Index =:= 1 + Last ->
    partial_find(Search, Binary, Index, SearchSize - 1).

partial_find(_Search, _Binary, _Index, 0) ->
    not_found;

partial_find(Search, Binary, Index, Total) ->
    <<Search1:Total/binary, _/binary>> = Search,
    case Binary of
        <<_Skip:Index/binary, Search1:Total/binary>> ->
            {partial, Index, Total};
        _ ->
            partial_find(Search, Binary, Index + 1, Total - 1)
    end.

bin_split(SplitBy, Binary) -> 
	bin_split(SplitBy, Binary, []).

bin_split(_SplitBy, <<>>, Acc) -> 
	lists:reverse(Acc);

bin_split(SplitBy, Binary, Acc) ->
	SplitSize = size(SplitBy),
	case bin_find(SplitBy, Binary) of
		{exact, 0} ->
			<<_:SplitSize/binary, Rest/binary>> = Binary,
			bin_split(SplitBy, Rest, Acc);
		{exact, Index} ->
			<<Part:Index/binary, _:SplitSize/binary, Rest/binary>> = Binary,
			bin_split(SplitBy, Rest, [Part | Acc]);
		{partial, _, _} ->
			lists:reverse([Binary | Acc]);
		not_found ->		
			lists:reverse([Binary | Acc])
	end.		

bin_to_list(Value) when is_binary(Value) ->
	binary_to_list(Value);
bin_to_list(Value) -> Value.

thing_to_bin(Value) when is_atom(Value) ->
	atom_to_binary(Value, utf8);
thing_to_bin(Value) when is_list(Value) ->
	list_to_binary(Value);
thing_to_bin(Value) when is_binary(Value) ->
	Value;
thing_to_bin(Value) ->
	term_to_binary(Value).

md5(Term, Base) ->
    <<SigInt:128/integer>> = erlang:md5(term_to_binary(Term)),
	Format = "~." ++ integer_to_list(Base) ++ "B",
    lists:flatten(io_lib:format(Format,[SigInt])).	