-module(yarmo_store).
-author('author <alex.portnov@gmail.com>').

-behaviour(data_store).

-include("yarmo.hrl").

-export([read/1, create/1, create/2, update/3, get_value/2, view/3, view/2]).

-define(DB_READ(Key), couchdb:retrieve_document(?DATABASE_NAME, Key)).
-define(DB_CREATE_ID(Key, Document), couchdb:create_document(?DATABASE_NAME, {Key, Document})).
-define(DB_CREATE(Document), couchdb:create_document(?DATABASE_NAME, Document)).
-define(DB_VIEW(DocName, ViewName, Options), couchdb:invoke_view(?DATABASE_NAME, DocName, ViewName, Options)).
-define(DB_REPLACE(Key, Rev, Document), couchdb:replace_document(?DATABASE_NAME, Key, Rev, Document)).

read(Key) ->
	case ?DB_READ(Key) of
		{json, {struct, [{<<"error">>,<<"not_found">>}, {<<"reason">>, _}]} } ->
			not_found;
		{json, {struct, Document} } -> Document
	end.	

create(Key, Document) ->
	case ?DB_CREATE_ID(Key, Document) of
		{json, {struct, [{<<"error">>,<<"conflict">>}| _]} }	-> {{id, ?l2b(Key)}, {rev, refetch}};
		{json, {struct, [{<<"ok">>, true},
		                 {<<"id">>, Id},
		                 {<<"rev">>, Rev}]} } -> {{id, Id}, {rev, Rev}}
	end.	

create(Document) ->
	{json,{struct, [{<<"ok">>, true},
	               {<<"id">>, Id},
	               {<<"rev">>, Rev}]} } = ?DB_CREATE(Document),
	{{id, Id}, {rev, Rev}}.

update(Key, OldRev, Document) ->
	case ?DB_REPLACE(Key, OldRev, Document) of
		{json, {struct, [{<<"error">>,<<"conflict">>} | _]} } -> {{id, ?l2b(Key)}, {rev, refetch}};
		{json, {struct, [{<<"error">>, Error} | _]} } -> {{id, ?l2b(Key)}, {rev, {bad_request, ?b2a(Error)} }};
		{json, {struct, [{<<"ok">>,true},
	               	     {<<"id">>, Id},
	               	     {<<"rev">>, Rev}]}} -> {{id, Id}, {rev, Rev}}
	end.	

view(DocName, ViewName) ->
	view(DocName, ViewName, []).

view(DocName, ViewName, Options) ->
	{json,{struct,[{<<"total_rows">>, _Total},
	               {<<"offset">>, _},
	               {<<"rows">>, Results}]} } = ?DB_VIEW(DocName, ViewName, Options),
	
	[M || {struct, [_, _, {<<"value">>, {struct, M} }] } <- Results].

get_value(Document, Name) ->	
	case lists:keysearch(?l2b(Name), 1, Document) of
		false -> [];
		{value, {_Name, Value}} -> Value
	end.