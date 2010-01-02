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
	Dest = #destination{type = "queue", name = ["sample", "queue", "example"], max_ttl = 800},
	
	#destination{
		id = "queue:sample.queue.example",
		type = "queue",
		name = ["sample","queue", "example"],
		max_ttl = 800,
		reply_time = 60
	} = Mod:create(Dest).
	
create_topic_test() ->
	Mod = test_mod([{create, {{id, "id"}, {rev, "Rev"}}}]),
	Dest = #destination{type = "topic", name = ["sample", "topic", "example"], max_ttl = 2800, reply_time = 200},

	#destination{
		id = "topic:sample.topic.example",
		type = "topic",
		name = ["sample", "topic", "example"],
		max_ttl = 2800,
		reply_time = 200
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

test_mod() ->
	test_mod([]).
		
test_mod(StoreOptions) ->
	Store = mock_store:new(StoreOptions),
	yarmo_destination:new(Store).	