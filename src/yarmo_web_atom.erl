-module(yarmo_web_atom).
-author('author <alex.portnov@gmail.com>').

-export([parse_atom_request/1, parse_link/1, parse_id/1, parse_content/1]).

-include_lib("xmerl/include/xmerl.hrl").

-include("yarmo.hrl").

parse_atom_request(#request{body = Body, context_root = ContextRoot}) ->
	{ Xml, _Rest } = xmerl_scan:string(Body),
	
	Fun = fun(#xmlElement{} = EntryNode, Acc) ->
		#content{type = Type, src = Src, body = MsgBody, summary = Summary} = parse_content(EntryNode),

		Links = case Src of
			undefined -> parse_link(EntryNode);
			Href      -> [#link{href = Href, rel = ["self"]} | parse_link(EntryNode)]
		end,
		
		Headers = [{'Content-Type', Type}, {'Message-Summary', Summary}, yarmo_link_util:link_header(Links)],	

		[#request{context_root = ContextRoot, method = 'POST',
		 	params = [{message_id, parse_id(EntryNode)}], headers = Headers, body = MsgBody} | Acc]
	end,
	
	lists:foldr(Fun, [], xmerl_xpath:string("//entry", Xml)).

parse_link(#xmlElement{} = EntryNode) ->
	LinkNodes = xmerl_xpath:string("//link", EntryNode),
	
	Fun = fun(#xmlElement{attributes = Attributes}, Acc) ->
		
		F = fun(#xmlAttribute{name = Name, value = Value}, L) ->
			case Name of
				href   -> L#link{href = to_list(Value)};
				title  -> L#link{title = to_list(Value)};
				anchor -> L#link{anchor = to_list(Value)};
				rel    -> L#link{rel = string:tokens(to_list(Value), " ")};
				Any    -> L#link{extensions = [{to_list(Any), to_list(Value)} | L#link.extensions]}
			end
		end,
		Link = lists:foldl(F, #link{}, Attributes),
		[Link | Acc]
	end,	
	
	lists:foldr(Fun, [], LinkNodes).

parse_id(#xmlElement{} = EntryNode)	->
	[ #xmlText{value = Id} ] = xmerl_xpath:string("id/text()", EntryNode),
	to_list(Id).

parse_content(#xmlElement{} = EntryNode) ->
	[ #xmlText{value = Summary} ] = xmerl_xpath:string("//summary/text()", EntryNode),	
	Content = #content{summary = Summary},
	
	Fun = fun(#xmlAttribute{name = Name, value = Value}, C) ->
		case Name of
			src  -> C#content{src = to_list(Value)};
			type -> C#content{type = to_list(Value)};
			_    -> C
		end	
	end,	
	
	case xmerl_xpath:string("//content", EntryNode) of
		[] -> Content;
		[#xmlElement{content = ElmText, attributes = Attributes} | _] -> 
		    Body = case ElmText of
				[#xmlText{value = B}] -> B;
				_ -> []
			end,	
			lists:foldl(Fun, Content#content{body = Body}, Attributes)		
	end.	
	
%% Utility Functions

to_list(V) ->
	if 
	 is_atom(V)	-> atom_to_list(V);
	 is_binary(V) -> binary_to_list(V);
	 true -> V	
	end.	
	
	