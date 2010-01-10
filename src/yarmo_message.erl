-module(yarmo_message, [Store]).
-author('author <alex.portnov@gmail.com>').

-include("yarmo.hrl").

-export([create/1, update/1, find/1, create_batch/1, find_batch/1, consume/1]).

%% For testing
-export([headers2json/1, json2headers/1, message2doc/1, doc2message/1]).

%% Public API

create(#message{} = Message) ->
	Document = [{?l2b("created_timestamp"), ?timestamp()} | message2doc(Message)],
	
	{{id, Id}, {rev, Rev}} = case Message#message.id of
		generated  -> Store:create(Document);
		Key        -> Store:create(Key, Document)
	end,	
	case Rev of 
		refetch -> find(Id);
		_       -> doc2message([{<<"_id">>, Id}, {<<"_rev">>, Rev} | Document])
	end.	

update(#message{id = Id, rev = OldRev} = Message) ->
	Document = message2doc(Message#message{id = undefined, rev = undefined}),
	BinId = ?l2b(Id),
	{{id, BinId}, {rev, Rev}} = Store:update(Id, OldRev, Document),
	case Rev of 
		{bad_request, Error} -> {bad_request, Error};
		refetch -> {conflict, refetch};
		_       -> {ok, {rev, ?b2l(Rev)}}
	end.	
	
find(MessageId) ->
	case Store:read(MessageId) of
		not_found -> not_found;
		Message -> doc2message(Message)
	end.		
	
create_batch(#batch{} = Batch) ->
	Document = [
		{?l2b("type"), ?l2b("batch")},
		{?l2b("destination"), ?l2b(Batch#batch.destination) },
		{?l2b("max_ttl"), Batch#batch.max_ttl },
		{?l2b("created_timestamp"), ?timestamp()}
	],
	{{id, Id}, {rev, Rev}} = Store:create(Document),
	doc2batch([{<<"_id">>, Id}, {<<"_rev">>, Rev} | Document]).	

find_batch(BatchId) ->
	case Store:read(BatchId) of
		not_found -> not_found;
		Batch -> doc2batch(Batch)
	end.		
	
consume(#destination{id = Id}) ->
	Key = fun(TimeStamp) -> ( [$[, $"] ++ Id ++ [$", $,, 32] ++ integer_to_list(TimeStamp) ++ [$]] ) end,	

	Options = [{limit, 1}, {descending, true}, {startkey, Key(?timestamp())}, {endkey, Key(0)}],
	
	case Store:view("message", "undelivered", Options) of
		[] -> not_found;
		[Message | _] -> doc2message(Message)
	end.	
	
%% Private API	
	
doc2message(Doc) ->
	BinFun = fun yarmo_bin_util:bin_to_list/1,	
	#message{
		id                = BinFun(Store:get_value(Doc, "_id")),
		rev		          = BinFun(Store:get_value(Doc, "_rev")),		
		destination       = BinFun(Store:get_value(Doc, "destination")),
		body              = BinFun(Store:get_value(Doc, "body")),
		max_ttl           = Store:get_value(Doc, "max_ttl"),
		headers           = json2headers(Store:get_value(Doc, "headers")),
		created_timestamp = Store:get_value(Doc, "created_timestamp")
	}.
	
message2doc(#message{} = Message) ->
	Properties = [
		{?l2b("type"), ?l2b("message")},
		{?l2b("destination"), ?l2b(Message#message.destination) },
		{?l2b("max_ttl"), Message#message.max_ttl },
		{?l2b("headers"), headers2json(filter_entity_headers(Message#message.headers)) },
		{?l2b("body"), Message#message.body },
		{?l2b("created_timestamp"), Message#message.created_timestamp},
		{?l2b("_rev"), Message#message.rev},
		{?l2b("_id"), Message#message.id}
	],
	
	Pred = fun({_Name, Value}) ->
		case Value of
			undefined -> false;
			generated -> false;
			_         -> true
		end	
	end,
	lists:filter(Pred, Properties).	

doc2batch(Doc) ->
	BinFun = fun yarmo_bin_util:bin_to_list/1,	
	#batch{
		id                = BinFun(Store:get_value(Doc, "_id")),
		rev		          = BinFun(Store:get_value(Doc, "_rev")),		
		destination       = BinFun(Store:get_value(Doc, "destination")),
		max_ttl           = Store:get_value(Doc, "max_ttl"),
		created_timestamp = Store:get_value(Doc, "created_timestamp")
	}.
	
filter_entity_headers(Headers) ->
	NoForward = [ 'Host', 'Content-Length', 'Accept-Encoding', 'Accept', 'User-Agent',
			'From', 'Accept-Language', 'Authorization', 'Charge-To', 'If-Modified-Since', 'Pragma'],

	Pred = fun({Name, Value}, L) ->
		case lists:member(Name, NoForward) of
			false ->
				[{Name, Value} | L];
			true  -> L
		end	
	end,	
	lists:foldl(Pred, [], Headers).	
	
headers2json(Headers) ->
	Fun = fun({Name, Value}, Acc) ->
		HeaderName = if
			is_atom(Name) -> ?a2b(Name);
			is_list(Name) -> ?l2b(Name);
			true -> Name
		end,	
		[{struct, [{name, HeaderName}, {value, ?l2b(Value)}]} | Acc]
	end,	
	lists:reverse(lists:foldl(Fun, [], Headers)).

json2headers(Headers) ->
	Fun = fun({struct, [{_, Name}, {_, Value}]}, Acc) ->
		[{?b2a(Name), ?b2l(Value)} | Acc]
	end,
	lists:reverse(lists:foldl(Fun, [], Headers)).				