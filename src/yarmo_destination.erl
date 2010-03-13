-module(yarmo_destination, [Store]).
-author('author <alex.portnov@gmail.com>').

-include("yarmo.hrl").

-export([find/1, find_all/0, create/1, generate_key/2]).
-export([subscribe/2, subscribe/3, unsubscribe/1, subscribers/1, deliver/2, subscriber/1]).

find(#destination{type = Type, name = Name}) ->
	case Store:read(generate_key(Type, Name)) of
		not_found -> not_found;
		Doc -> doc2dest(Doc)
	end.	
	
find_all() ->
	lists:map(fun(Doc) -> doc2dest(Doc) end, Store:view("destination", "all")).	

create(#destination{type = Type, name = Name} = Destination) ->
	Document = [
		{?l2b("type"), ?l2b(Type)},
		{?l2b("name"), ?l2b(string:join(Name, "."))},
		{?l2b("max_ttl"), Destination#destination.max_ttl},
		{?l2b("reply_time"), Destination#destination.reply_time},
		{?l2b("ack_mode"), ?l2b(Destination#destination.ack_mode)},
		{?l2b("created_timestamp"), ?timestamp()}
	],
	Key = generate_key(Type, Name),
	{{id, _}, {rev, Rev}} = Store:create(Key, Document),
	
	doc2dest([{<<"_id">>, Key}, {<<"_rev">>, Rev} | Document]).

generate_key(Type, Name) ->
	Type ++ ":" ++ string:join(Name, ".").	

subscribe(#destination{type = "topic"} = Destination, Subscriber) ->
	subscribe(Destination, Subscriber, "false").	
	
subscribe(#destination{type = "topic", id = DestId}, Subscriber, POE) ->
	Timestamp = ?timestamp(),
	Document = [
		{?l2b("type"), <<"subscription">>},
		{?l2b("destination"), ?l2b(DestId)},
		{?l2b("subscriber"), ?l2b(Subscriber)},
		{?l2b("poe"), ?l2b(POE)},
		{?l2b("created_timestamp"), Timestamp}
	],
	{{id, Id}, {rev, Rev}} = Store:create(Document),
	#subscription{id = ?b2l(Id), rev = ?b2l(Rev), destination = DestId, 
		subscriber = Subscriber, poe = POE, created_timestamp = Timestamp}.

unsubscribe(#subscription{id = Id} = Subscription) -> 
	case Store:read(Id) of
		not_found -> {ok, not_found};
		Doc -> 
			Rev = ?b2l(Store:get_value(Doc, "_rev")),
			case Store:delete(Id, Rev)	of
				{error, refetch} -> unsubscribe(Subscription);
				Any -> Any
			end		
	end.	
	
subscriber(ID) ->	
	case Store:read(ID) of
		not_found -> not_found;
		Doc -> Fun = doc2subscription(), Fun(Doc)
	end.	
	
subscribers(#destination{id = Id}) ->
	Key = [$[, $"] ++ Id ++ [$", $]],
	Options = [{key, Key}],
	lists:map(doc2subscription(), Store:view("destination", "subscribers", Options)).
	
%% SendFun for now should be a wrapper for ibrowse:send_req	
deliver(#message{destination = Destination, headers = Headers, body = Body}, SendFun) ->
	Subscribers = subscribers(#destination{id = Destination}),

	DeliveryFun = fun(#subscription{subscriber = Subscriber, poe = _POE}) ->
		case SendFun(Subscriber, Headers, post, Body) of
			{ok, _, _, _}   -> {ok, delivered};
			{error, Reason} -> {error, Reason}
		end
	end,			
	lists:foreach(fun(S) -> spawn(fun() -> DeliveryFun(S) end) end, Subscribers).	
	
doc2dest(Doc) ->
	BinFun = fun yarmo_bin_util:bin_to_list/1,	
	
	#destination{
		id         = BinFun(Store:get_value(Doc, "_id")),
		rev		   = BinFun(Store:get_value(Doc, "_rev")),
		type       = BinFun(Store:get_value(Doc, "type")),
		name       = string:tokens(BinFun(Store:get_value(Doc, "name")), "."),
		max_ttl    = Store:get_value(Doc, "max_ttl"),
		reply_time = Store:get_value(Doc, "reply_time"),
		ack_mode   = BinFun(Store:get_value(Doc, "ack_mode"))
	}.	

doc2subscription() ->
	fun(Doc) ->
		Val = fun(Name) -> ?b2l(Store:get_value(Doc, Name)) end,
		#subscription{
			id = Val("_id"), 
			rev = Val("_rev"), 
			destination = Val("destination"),
			subscriber = Val("subscriber"), 
			poe = Val("poe")
		}
	end.	