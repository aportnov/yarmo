-module(destination_test).
-author('author <alex.portnov@gmail.com>').

-include_lib("eunit/include/eunit.hrl").

-include("../src/yarmo.hrl").

-define(TEST_MOD, yarmo_destination).

generate_key_test() ->
	"queue:sample.queue.example" = ?TEST_MOD:generate_key("queue", ["sample", "queue", "example"]),
	"topic:sample.queue.example" = ?TEST_MOD:generate_key("topic", ["sample", "queue", "example"]),
	"topic:sample" = ?TEST_MOD:generate_key("topic", ["sample"]).
	
create_queue_test() ->
	Store = mock_store:new([{create, {{id, "id"}, {rev, "Rev"}}}]),
	Dest = #destination{type = "queue", name = ["sample", "queue", "example"], max_ttl = 800},
	
	#destination{
		id = "queue:sample.queue.example",
		type = "queue",
		name = ["sample","queue", "example"],
		max_ttl = 800,
		reply_time = 60
	} = ?TEST_MOD:create(Store, Dest).
	
create_topic_test() ->
	Store = mock_store:new([{create, {{id, "id"}, {rev, "Rev"}}}]),
	Dest = #destination{type = "topic", name = ["sample", "topic", "example"], max_ttl = 2800, reply_time = 200},

	#destination{
		id = "topic:sample.topic.example",
		type = "topic",
		name = ["sample", "topic", "example"],
		max_ttl = 2800,
		reply_time = 200
	} = ?TEST_MOD:create(Store, Dest).

find_destination_not_existing_test() ->
	Store = mock_store:new([{read, not_found}]),
	
	Dest = #destination{type = "topic", name = ["sample", "topic", "example"]},
	
	not_found = ?TEST_MOD:find(Store, Dest).
	
find_destination_test() ->
	Document = [
		{<<"_id">>, ?l2b("queue:sample.queue.example")},
		{?l2b("type"), ?l2b("queue")},
		{?l2b("name"), ?l2b("sample.queue.example")},
		{?l2b("max_ttl"), 300},
		{?l2b("reply_time"), 500},
		{?l2b("created_timestamp"), calendar:datetime_to_gregorian_seconds(erlang:universaltime())}
	],
	
	Store = mock_store:new([{read, Document}]), 	
			
	Dest = #destination{type = "queue", name = ["sample", "queue", "example"]},

	#destination{
		id = "queue:sample.queue.example",
		type = "queue",
		name = ["sample","queue", "example"],
		max_ttl = 300,
		reply_time = 500
	} = ?TEST_MOD:find(Store, Dest).	