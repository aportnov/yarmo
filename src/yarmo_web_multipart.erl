-module(yarmo_web_multipart).
-author('author <alex.portnov@gmail.com>').

-export([parse_multipart_request/1]).

%% Export for testing
-export([find_in_binary/2, multipart_boundary/1, split_bin/2]).

-include("yarmo.hrl").

%% Public API

parse_multipart_request(#request{body = Body} = Request) ->
	case multipart_boundary(Request) of
		not_found -> [];
		Value ->
			Boundary = ?l2b(Value),
			parser_multipart_request(fix_incomplite_body(Body, Boundary), Boundary)
	end.		

%% Private API Implementation

parser_multipart_request(Body, Boundary) ->
	BS = byte_size(Boundary),
	RequestBody = case find_in_binary(<<"\r\n--", Boundary:BS/binary, "--\r\n">>, Body) of
		{exact, Index} ->
			<<B:Index/binary, _/binary>> = Body,
			B;
		_ -> <<>>
	end,
	
	Requests = split_bin(<<"--", Boundary:BS/binary, "\r\n">>, RequestBody),
	Fun = fun(ReqBin, Acc) ->
		[parse_request_part(ReqBin) | Acc]
	end,
	lists:foldr(Fun, [], Requests).			

parse_request_part(Binary) ->
	[Body | Headers] = lists:reverse(split_bin(<<"\r\n">>, Binary)),

	Fun = fun(Line, Acc) -> 
		[split_header(Line) | Acc]
	end,
	#request{headers = lists:foldr(Fun, [], Headers), body = Body, method = 'POST'}.

%% Utility Functions

split_bin(SplitBy, Binary) -> 
	split_bin(SplitBy, Binary, []).
	
split_bin(_SplitBy, <<>>, Acc) -> 
	lists:reverse(Acc);
		
split_bin(SplitBy, Binary, Acc) ->
	SplitSize = size(SplitBy),
	case find_in_binary(SplitBy, Binary) of
		{exact, 0} ->
			<<_:SplitSize/binary, Rest/binary>> = Binary,
			split_bin(SplitBy, Rest, Acc);
		{exact, Index} ->
			<<Part:Index/binary, _:SplitSize/binary, Rest/binary>> = Binary,
			split_bin(SplitBy, Rest, [Part | Acc]);
		not_found ->		
			lists:reverse([Binary | Acc])
	end.		

split_header(Line) ->
    {Name, [$: | Value]} = lists:splitwith(fun (C) -> C =/= $: end,
                                           binary_to_list(Line)),
	Header = case mochiweb_util:parse_header(Value) of
		{H, []} -> H;
		H -> H
	end,										
    {?l2a(string:strip(Name)), Header}.

find_in_binary(Search, Binary) when size(Search) > 0 ->
    case size(Binary) - size(Search) of
        Last when Last < 0 ->
            partial_find(Search, Binary, 0, size(Binary));
        Last ->
            find_in_binary(Search, size(Search), Binary, 0, Last)
    end.

find_in_binary(Search, SearchSize, Binary, Index, Last) when Index =< Last->
    case Binary of
        <<_:Index/binary, Search:SearchSize/binary, _/binary>> ->
            {exact, Index};
        _ ->
            find_in_binary(Search, SearchSize, Binary, Index + 1, Last)
    end;

find_in_binary(Search, SearchSize, Binary, Index, Last) when Index =:= 1 + Last ->
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

multipart_boundary(#request{headers = Headers}) ->
	case lists:keysearch('Content-Type', 1, Headers) of
		{value, {_, ContentType}}  -> 
			case mochiweb_util:parse_header(ContentType) of
				{[$m,$u,$l,$t,$i,$p,$a,$r,$t, $/ | _], Opts } ->
					case proplists:get_value("boundary", Opts) of
						Boundary when is_list(Boundary) -> Boundary;
						_ -> not_found
					end;	
				_ -> not_found	
			end;		
		_ -> not_found	
	end.
	
fix_incomplite_body(Buffer, Boundary) ->	
	PrefixSize = size(Boundary),
	case size(Buffer) - (2 + PrefixSize) of
	    Seek when Seek >= 0 ->
	        case Buffer of
	            <<_:Seek/binary, Boundary:PrefixSize/binary, "--">> ->
	                <<Buffer/binary, "\r\n">>;
	            _ ->
	                Buffer
	        end;
	    _ ->
	        Buffer
	end.

