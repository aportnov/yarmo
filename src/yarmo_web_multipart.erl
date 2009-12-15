-module(yarmo_web_multipart).
-author('author <alex.portnov@gmail.com>').

-export([parse_multipart_request/1]).

%% Export for testing
-export([multipart_boundary/1]).

-include("yarmo.hrl").

%% Public API
parse_multipart_request(#request{body = Body} = Request) when is_list(Body) ->
	parse_multipart_request(Request#request{body = ?l2b(Body)});

parse_multipart_request(#request{body = B} = Request) ->
	case multipart_boundary(Request) of
		not_found -> [];
		Value ->
			Boundary = ?l2b(Value),
			Mod = yarmo_bin_util,
			Body = Mod:bin_replace(Mod:bin_replace(B, <<"\\r">>, <<"\r">>), <<"\\n">>, <<"\n">>),
			parser_multipart_request(fix_incomplite_body(Body, Boundary), Boundary)
	end.		

%% Private API Implementation

parser_multipart_request(Body, Boundary) ->
	BS = byte_size(Boundary),
	RequestBody = case yarmo_bin_util:bin_find(<<"\r\n--", Boundary:BS/binary, "--\r\n">>, Body) of
		{exact, Index} ->
			<<B:Index/binary, _/binary>> = Body,
			B;
		_ -> <<>>
	end,
	
	Requests = yarmo_bin_util:bin_split(<<"--", Boundary:BS/binary, "\r\n">>, RequestBody),
	Fun = fun(ReqBin, Acc) ->
		[parse_request_part(ReqBin) | Acc]
	end,
	lists:foldr(Fun, [], Requests).			

parse_request_part(Binary) ->
	[Body | Headers] = lists:reverse(yarmo_bin_util:bin_split(<<"\r\n">>, Binary)),

	Fun = fun(Line, Acc) -> 
		[split_header(Line) | Acc]
	end,
	#request{headers = lists:foldr(Fun, [], Headers), body = Body, method = 'POST'}.

%% Utility Functions

split_header(Line) ->
    {Name, [$: | Value]} = lists:splitwith(fun (C) -> C =/= $: end,
                                           binary_to_list(Line)),
	Header = case mochiweb_util:parse_header(Value) of
		{H, []} -> H;
		H -> H
	end,										
    {?l2a(string:strip(Name)), Header}.

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
