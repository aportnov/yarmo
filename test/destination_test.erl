-module(destination_test).
-author('author <alex.portnov@gmail.com>').

-include_lib("eunit/include/eunit.hrl").

-include("../src/yarmo.hrl").

generate_key_test() ->
	Mod = test_mod(),
	"queue:sample.queue.example" = Mod:generate_key("queue", ["sample", "queue", "example"]),
	"topic:sample.queue.example" = Mod:generate_key("topic", ["sample", "queue", "example"]),
	"topic:sample" = Mod:generate_key("topic", ["sample"]).
	
create_queue_test() ->
	Mod = test_mod([{create, {{id, "id"}, {rev, "Rev"}}}]),
	Dest = #destination{type = "queue", name = ["sample", "queue", "example"], max_ttl = 800, ack_mode = "single"},
	
	#destination{
		id = "queue:sample.queue.example",
		type = "queue",
		name = ["sample","queue", "example"],
		max_ttl = 800,
		reply_time = 60,
		ack_mode = "single"
	} = Mod:create(Dest).
	
create_topic_test() ->
	Mod = test_mod([{create, {{id, "id"}, {rev, "Rev"}}}]),
	Dest = #destination{type = "topic", name = ["sample", "topic", "example"], max_ttl = 2800, reply_time = 200},

	#destination{
		id = "topic:sample.topic.example",
		rev = "Rev",
		type = "topic",
		name = ["sample", "topic", "example"],
		max_ttl = 2800,
		reply_time = 200,
		ack_mode = "auto"
	} = Mod:create(Dest).

find_destination_not_existing_test() ->
	Mod = test_mod([{read, not_found}]),
	
	Dest = #destination{type = "topic", name = ["sample", "topic", "example"]},
	
	not_found = Mod:find(Dest).
	
find_destination_test() ->
	Document = [
		{<<"_id">>, ?l2b("queue:sample.queue.example")},
		{?l2b("type"), ?l2b("queue")},
		{?l2b("name"), ?l2b("sample.queue.example")},
		{?l2b("max_ttl"), 300},
		{?l2b("reply_time"), 500},
		{?l2b("created_timestamp"), calendar:datetime_to_gregorian_seconds(erlang:universaltime())}
	],
	
	Mod = test_mod([{read, Document}]), 	
			
	Dest = #destination{type = "queue", name = ["sample", "queue", "example"]},

	#destination{
		id = "queue:sample.queue.example",
		type = "queue",
		name = ["sample","queue", "example"],
		max_ttl = 300,
		reply_time = 500
	} = Mod:find(Dest).	
	
find_all_destinations_test() ->
	Document = [
		{<<"_id">>, ?l2b("queue:sample.queue.example")},
		{?l2b("type"), ?l2b("queue")},
		{?l2b("name"), ?l2b("sample.queue.example")},
		{?l2b("max_ttl"), 300},
		{?l2b("reply_time"), 500},
		{?l2b("created_timestamp"), 111111}
	],
	
	Mod = test_mod([{view, [Document]}]), 	
	
	[
		#destination{
			id = "queue:sample.queue.example",
			type = "queue",
			name = ["sample","queue", "example"],
			max_ttl = 300,
			reply_time = 500
		}
	] = Mod:find_all().	

topic_subscribe_test_() ->
	Mod = test_mod([{create, {{id, <<"sub-id">>}, {rev, <<"Rev">>}}}]),
	Dest = #destination{type = "topic", name = ["sample", "topic", "example"], id = "topic:sample.topic.example"},
	
	Subscription = #subscription{
		id = "sub-id", rev = "Rev", destination = "topic:sample.topic.example",
		subscriber = "http://www.somesite.com/subscriber/333", poe = "false"
	},
	
	Match = fun(#subscription{} = Source, #subscription{} = Target) ->
		?assertEqual(Source#subscription.id, Target#subscription.id),
		?assertEqual(Source#subscription.rev, Target#subscription.rev),
		?assertEqual(Source#subscription.destination, Target#subscription.destination),
		?assertEqual(Source#subscription.subscriber, Target#subscription.subscriber),
		?assertEqual(Source#subscription.poe, Target#subscription.poe)
	end,	
	[
    	fun() -> 
			Match(Subscription, Mod:subscribe(Dest, "http://www.somesite.com/subscriber/333")) 
		end,
		fun() -> 
			Match(Subscription#subscription{poe = "true"}, Mod:subscribe(Dest, "http://www.somesite.com/subscriber/333", "true")) 
		end
    ].

topic_subscribers_test() ->
	ViewFun = fun(["destination", "subscribers", [{key,"[\"topic:sample.topic\"]"}]]) -> 
		[
			[
				{<<"_id">>, <<"sub-id">>},
			 	{<<"_rev">>, <<"Rev">>},
			 	{<<"destination">>, <<"topic:sample.topic">>},
			 	{<<"subscriber">>, <<"http://some-url.com">>},
				{<<"poe">>, <<"false">>}
			]
		]
	end,
	Mod = test_mod([{view, ViewFun}]),
	[#subscription{
		id = "sub-id", 
		rev = "Rev", 
		destination = "topic:sample.topic", 
		subscriber = "http://some-url.com", 
		poe = "false"
	}] = Mod:subscribers(#destination{id = "topic:sample.topic"}).	

find_subscriber_test_() ->
	[
		fun() ->
			Mod = test_mod([{read, not_found}]),
		    ?assertEqual(not_found, Mod:subscriber("some"))	
		end,
		fun() ->
			Timestamp = ?timestamp(),
			Doc = [
			    {<<"_id">>, <<"sub-id">>},
			    {<<"_rev">>, <<"sub-rev">>},
				{<<"type">>, <<"subscription">>},
				{<<"destination">>, <<"topic:some.topic">>},
				{<<"subscriber">>, <<"http://www.some.com">>},
				{<<"poe">>, <<"false">>},
				{<<"created_timestamp">>, Timestamp}
			],
			Sub = #subscription{id = "sub-id", rev = "sub-rev", 
								destination = "topic:some.topic",
								subscriber = "http://www.some.com", poe = "false"},
			Mod = test_mod([{read, Doc}]),
			?assertEqual(Sub, Mod:subscriber("sub-id"))
		end		
	].

deliver_message_topic_subscribers_test() ->
	ViewFun = fun(["destination", "subscribers", [{key,"[\"topic:sample.topic\"]"}]]) -> 
		[
			[
				{<<"_id">>, <<"sub-id">>},
			 	{<<"_rev">>, <<"Rev">>},
			 	{<<"destination">>, <<"topic:sample.topic">>},
			 	{<<"subscriber">>, <<"http://some-url.com">>},
				{<<"poe">>, <<"false">>}
			]
		]
	end,
	Mod = test_mod([{view, ViewFun}]),
	
	Message = #message{headers = [{'hello', "hello"}], body = "sample", destination = "topic:sample.topic"},
	
	SendFun = fun(Subscriber, Headers, post, Body) ->
		?assertEqual("http://some-url.com", Subscriber),
		?assertEqual(Message#message.headers, Headers),
		?assertEqual(Message#message.body, Body),
		{ok, 200, [], []}
	end,
	Mod:deliver(Message, SendFun).	

test_mod() ->
	test_mod([]).
		
test_mod(StoreOptions) ->
	Store = mock_store:new(StoreOptions),
	yarmo_destination:new(Store).	