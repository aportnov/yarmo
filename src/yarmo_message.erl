-module(yarmo_message, [Store]).
-author('author <alex.portnov@gmail.com>').

-include("yarmo.hrl").

-export([create/1, update/1, find/1, consume/1, consume/2, acknowledge/1]).
-export([create_batch/1, find_batch/1]).
-export([create_poe_message/2, update_poe_message/2]).

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
	
create_poe_message(#destination{id = DestId, max_ttl = MaxTtl}, POE) ->
	Document = [
		{?l2b("type"), ?l2b("poe-message")},
		{?l2b("destination"), ?l2b(DestId) },
		{?l2b("max_ttl"), MaxTtl },
		{?l2b("poe"), ?l2b(POE)},
		{?l2b("created_timestamp"), ?timestamp()}
	],	
	{{id, Id}, {rev, _Rev}} = Store:create(Document),
	{id, ?b2l(Id)}.
	
update_poe_message(#message{id = Id} = Message, POE) ->
	BinPoe = ?l2b(POE),	
	case Store:read(Id) of
		not_found -> not_found;
		Doc ->
		  case Store:get_value(Doc, "poe") of
		  	BinPoe ->
				Rev = Store:get_value(Doc, "_rev"),
				update(Message#message{rev = Rev});
			_   ->	{bad_request, poe_missmatch}
		  end		
	end.			
	
consume(#destination{type = "queue"} = Destination) ->
	Callback = fun(#message{} = Messsage) ->
		case update(Messsage#message{consumed_timestamp = ?timestamp()}) of
			{ok, {rev, Rev}}     -> Messsage#message{rev = Rev};
			{conflict, refetch}  -> consume(Destination);
			{bad_request, Error} -> {error, Error}
		end	
	end,
	consume(Destination, Callback);
			
consume(#destination{type = "topic"} = Destination) ->
	consume(Destination, fun(#message{} = M) -> M end).
	
consume(#destination{id = Id}, Callback) ->
	Key = fun(TimeStamp) -> ( [$[, $"] ++ Id ++ [$", $,, 32] ++ integer_to_list(TimeStamp) ++ [$]] ) end,	

	Options = [{limit, 1}, {descending, true}, {startkey, Key(?timestamp())}, {endkey, Key(0)}],
	
	case Store:view("message", "undelivered", Options) of
		[] -> not_found;
		[Message | _] -> Callback(doc2message(Message))
	end.	

acknowledge(#message{id = Id}) ->
	case find(Id) of
		not_found -> not_found;
		#message{consumed_timestamp = undefined, rev = Rev} -> {not_consumed, Rev};
		#message{acknowledged_timestamp = undefined} = Msg ->
			case update(Msg#message{acknowledged_timestamp = ?timestamp()}) of
				{ok, {rev, Rev}}     -> {acknowledged, Rev};
				{conflict, refetch}  -> acknowledge(Msg);
				{bad_request, Error} -> {error, Error}
		    end; 
		#message{rev = Rev} -> {acknowledged, Rev}
	end.	
	
%% Private API	
	
doc2message(Doc) ->
	BinFun = fun yarmo_bin_util:bin_to_list/1,	
	
	Filter = fun(Value) ->
		case Value of 
			[] -> undefined; 
			Any -> Any
		end
	end,		
		
	#message{
		id                     = BinFun(Store:get_value(Doc, "_id")),
		rev		               = BinFun(Store:get_value(Doc, "_rev")),		
		destination            = BinFun(Store:get_value(Doc, "destination")),
		body                   = BinFun(Store:get_value(Doc, "body")),
		max_ttl                = Store:get_value(Doc, "max_ttl"),
		headers            	   = json2headers(Store:get_value(Doc, "headers")),
		created_timestamp      = Store:get_value(Doc, "created_timestamp"),
		consumed_timestamp     = Filter(BinFun(Store:get_value(Doc, "consumed_timestamp"))),
		acknowledged_timestamp = Filter(BinFun(Store:get_value(Doc, "acknowledged_timestamp")))
	}.
	
message2doc(#message{} = Message) ->
	
	Properties = [
		{?l2b("type"), ?l2b("message")},
		{?l2b("destination"), ?l2b(Message#message.destination) },
		{?l2b("max_ttl"), Message#message.max_ttl },
		{?l2b("headers"), headers2json(filter_entity_headers(Message#message.headers)) },
		{?l2b("body"), yarmo_bin_util:thing_to_bin(Message#message.body) },
		{?l2b("created_timestamp"), Message#message.created_timestamp},
		{?l2b("consumed_timestamp"), Message#message.consumed_timestamp},
		{?l2b("acknowledged_timestamp"), Message#message.acknowledged_timestamp},
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