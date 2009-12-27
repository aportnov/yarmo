-module(yarmo_web_atom).
-author('author <alex.portnov@gmail.com>').

-export([parse_atom_request/1, parse_link/1, parse_id/1]).

-include_lib("xmerl/include/xmerl.hrl").

-include("yarmo.hrl").

parse_atom_request(#request{body = Body} = Request) ->
	[].

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


	
%% Utility Functions

to_list(V) ->
	if 
	 is_atom(V)	-> atom_to_list(V);
	 is_binary(V) -> binary_to_list(V);
	 true -> V	
	end.	
	
	