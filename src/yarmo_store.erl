-module(yarmo_store).
-author('author <alex.portnov@gmail.com>').

-behaviour(data_store).

-include("yarmo.hrl").

-export([read/1, create/1, create/2, get_value/2]).

read(Key) ->
	case ?DB_READ(?DATABASE_NAME, Key) of
		{json, {struct, [{<<"error">>,<<"not_found">>}, {<<"reason">>, _}]} } ->
			not_found;
		{json, {struct, Document} } -> Document
	end.	

create(Key, Document) ->
	{json,{struct, [{<<"ok">>, true},
	               {<<"id">>, Id},
	               {<<"rev">>, Rev}]} } = ?DB_CREATE_ID(?DATABASE_NAME, Key, Document),
	{{id, Id}, {rev, Rev}}.

create(Document) ->
	{json,{struct, [{<<"ok">>, true},
	               {<<"id">>, Id},
	               {<<"rev">>, Rev}]} } = ?DB_CREATE(?DATABASE_NAME, Document),
	{{id, Id}, {rev, Rev}}.

get_value(Document, Name) ->	
	case lists:keysearch(?l2b(Name), 1, Document) of
		false -> [];
		{value, {_Name, Value}} -> Value
	end.	