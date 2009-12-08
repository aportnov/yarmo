-module(erlymsg_web_utils).
-author('author <alex.portnov@gmail.com>').

-export([link_header_builder/2, get_header/3]).

link_header_builder(Relationships, Prefix) ->
	fun(Destination, Host) ->
		BasePath = [Prefix | Destination],

		Fun = fun({{rel, Rel}, {path, Suffix}}, Acc) ->
			Path = "/" ++ string:join(BasePath ++ [Suffix], "/"),
			Link = "<http://" ++ Host ++ Path ++ ">; rel=\"" ++ Rel ++ "\"",
			[Link | Acc]
		end,
		Link = string:join(lists:foldr(Fun, [], Relationships), ", "),
		
		{'Link', Link}
	end.	

	
get_header(Name, Default, Headers) ->
	case lists:keysearch(Name, 1, Headers) of
		{value, {Name, Value}}  -> Value;
		_ -> Default	
	end.	
			