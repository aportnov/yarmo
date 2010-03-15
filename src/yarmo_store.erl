-module(yarmo_store).
-author('author <alex.portnov@gmail.com>').

-behaviour(data_store).

-include("yarmo.hrl").

-export([read/1, create/1, create/2, delete/2, bulk_delete/1, update/3, get_value/2]).
-export([view/3, view/2, create_view/2]).

-define(DB_READ(Key), couchdb:retrieve_document(?DB_HOST, ?DATABASE_NAME, Key)).
-define(DB_CREATE_ID(Key, Document), couchdb:create_document(?DB_HOST,?DATABASE_NAME, {Key, Document})).
-define(DB_CREATE(Document), couchdb:create_document(?DB_HOST, ?DATABASE_NAME, Document)).
-define(DB_VIEW(DocName, ViewName, Options), couchdb:invoke_view(?DB_HOST, ?DATABASE_NAME, DocName, ViewName, Options)).
-define(DB_REPLACE(Key, Rev, Document), couchdb:replace_document(?DB_HOST, ?DATABASE_NAME, Key, Rev, Document)).
-define(DB_DELETE(Key, Rev), couchdb:delete_document(?DB_HOST, ?DATABASE_NAME, Key, Rev)).
-define(DB_DELETE_BULK(L), erlang_couchdb:delete_documents(?DB_HOST, ?DATABASE_NAME, L)).
-define(DB_CREATE_VIEW(ViewName, Views, Attributes), 
		erlang_couchdb:create_view(?DB_HOST, ?DATABASE_NAME, ViewName, <<"javascript">>, Views, Attributes)).

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

delete(Key, Rev) ->
	case ?DB_DELETE(Key, Rev) of
		{json, {struct, [{<<"error">>,<<"conflict">>} | _]} }  -> {error, refetch};
		{json, {struct, [{<<"error">>,<<"not_found">>} | _]} } -> {ok, not_found};
		{json, {struct, [{<<"error">>, Error} | _]} }          -> {error, ?b2a(Error)};
		{json, {struct, [{<<"ok">>,true} | _]} }               -> {ok, deleted}
	end.

bulk_delete(Documents) ->
	F = fun yarmo_bin_util:thing_to_bin/1, 
	
	{json, Results} = ?DB_DELETE_BULK([{F(Id), F(Rev)} || {Id, Rev} <- Documents]),
	Pred = fun({struct, Attributes}) ->
		case Attributes of
			[{<<"id">>, Id}, {<<"error">>, Error} | _] -> {error, ?b2l(Id), ?b2a(Error)};
			[{<<"id">>, Id}, {<<"rev">>, _Rev} | _]    -> {ok, ?b2l(Id)}
		end	
	end,
	lists:map(Pred, Results).

view(DocName, ViewName) ->
	view(DocName, ViewName, []).

view(DocName, ViewName, Options) ->
	{json,{struct,[{<<"total_rows">>, _Total},
	               {<<"offset">>, _},
	               {<<"rows">>, Results}]} } = ?DB_VIEW(DocName, ViewName, Options),
	
	lists:map(fun({struct, [_, _, {<<"value">>, Value }] }) -> 
		case Value of {struct, M} -> M; Any -> Any end	
	end, Results).
	
create_view(ViewName, Views) ->	
	Attributes = case read("_design/" ++ ViewName) of
		not_found -> [];
		Doc -> [{<<"_rev">>, get_value(Doc, "_rev")}]
	end,
	{json,{struct, [{<<"ok">>, true},
	               {<<"id">>, Id},
	               {<<"rev">>, Rev}]} } = ?DB_CREATE_VIEW(ViewName, Views, Attributes),
	{{id, Id}, {rev, Rev}}.	

get_value(Document, Name) ->	
	case lists:keysearch(?l2b(Name), 1, Document) of
		false -> [];
		{value, {_Name, Value}} -> Value
	end.