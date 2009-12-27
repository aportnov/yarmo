-module(yarmo_link_util).
-author('author <alex.portnov@gmail.com>').

-include("yarmo.hrl").

-compile(export_all).

%% Public API
link_header(LinkRecords) ->
	Fun = fun(#link{} = Link, Acc) ->
		[parse_link(Link) | Acc]
	end,	

	Links = lists:reverse(lists:foldl(Fun, [], LinkRecords)),

	{'Link', string:join(Links, ", ")}.
	
parse_link(#link{} = Link) ->
    Fun = fun(Option, Acc) ->
		erlang:apply(?MODULE, list_to_atom("parse_" ++ Option), [Link, Acc])
	end,
	Parts = lists:foldr(Fun, [], ["href", "rel", "title", "anchor", "extensions"]),

	Pred = fun(Elm) -> 
		case Elm of 
			undefined -> false;
			_ -> true 
		end 
	end,
	string:join(lists:filter(Pred, Parts), "; ").

%% Private API	
	
parse_href(#link{href = Href}, Acc) ->
	["<" ++ Href ++ ">" | Acc].	
	
parse_rel(#link{rel = Rel}, Acc) ->
	[parse_option(Rel, "rel", fun(L) -> ("\"" ++ string:join(L, " ") ++ "\"") end) | Acc].

parse_title(#link{title = Title}, Acc) ->
	[parse_option(Title, "title") | Acc].

parse_anchor(#link{anchor = Anchor}, Acc) ->
	[parse_option(Anchor, "anchor") | Acc].

parse_extensions(#link{extensions = Extensions}, Acc) ->
	Fun = fun({Name, Value}, Res) ->
		[parse_option(Value, Name) | Res]
	end,	
	lists:foldl(Fun, Acc, Extensions).
	
parse_option(Value, Name) ->
	parse_option(Value, Name, fun(L) -> ("\"" ++ L ++ "\"") end).
	
parse_option(Value, Name, Callback) ->
	case Value of
		[] -> undefined;
		undefined -> undefined;
		Option -> Name ++ "=" ++ Callback(Option)
	end.		
			