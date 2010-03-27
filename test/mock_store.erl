-module(mock_store, [MockStore]).
-author('author <alex.portnov@gmail.com>').

-behaviour(data_store).

-export([read/1, create/1, create/2, delete/2, bulk_delete/1, update/3, get_value/2]).
-export([view/3, view/2, create_view/2]).
-export([create_db/0]).

read(Key) ->
	callback(read, [Key]).

create(Key, Document) ->
	callback(create, [Key, Document]).

create(Document) ->
	callback(create, [Document]).

get_value(Document, Name) ->	
	case lists:keysearch(list_to_binary(Name), 1, Document) of
		false -> [];
		{value, {_Name, Value}} -> Value
	end.

view(DocName, ViewName) ->
	callback(view, [DocName, ViewName]).

view(DocName, ViewName, Options) ->
	callback(view, [DocName, ViewName, Options]).
	
update(Key, OldRev, Document) ->
	callback(update, [Key, OldRev, Document]).	

delete(Key, Rev) ->
	callback(delete, [Key, Rev]).
	
bulk_delete(Documents) ->
	callback(bulk_delete, [Documents]).

create_view(ViewName, Views) ->
	callback(create_view, [ViewName, Views]).	
			
create_db() ->
	callback(create_db, []).
				
%% Helper Functions

callback(Name, Args) ->
	case proplists:lookup(Name, MockStore) of
		none -> undefined;
		{Name, Fun} when is_function(Fun) -> Fun(Args);
		{Name, Value} -> Value
	end.