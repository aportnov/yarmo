-module(mock_store, [MockStore]).
-author('author <alex.portnov@gmail.com>').

-behaviour(data_store).

-export([read/1, create/1, create/2, get_value/2]).

read(_Key) ->
	{{read, Result}, _, _} = MockStore,
	Result.	

create(_Key, _Document) ->
	{_, {create, Result}, _} = MockStore,
	Result.	


create(_Document) ->
	{_, _, {create, Result}} = MockStore,
	Result.	

get_value(Document, Name) ->	
	case lists:keysearch(list_to_binary(Name), 1, Document) of
		false -> [];
		{value, {_Name, Value}} -> Value
	end.