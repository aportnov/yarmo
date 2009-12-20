-module(message_test).
-author('author <alex.portnov@gmail.com>').

-include_lib("eunit/include/eunit.hrl").

-include("../src/yarmo.hrl").

-define(TEST_MOD, yarmo_message).

header_conversion_test() ->
	Headers = [{'Host', "www.sample.com"}, {'X-Powered-By', "Erlang"}],
	JsonHeaders = [{struct,[{name, <<"Host">>}, {value, <<"www.sample.com">>}]}, {struct, [{name, <<"X-Powered-By">>}, {value, <<"Erlang">>}]}],
	
 	JsonHeaders = ?TEST_MOD:headers2json(Headers),
	Headers     = ?TEST_MOD:json2headers(JsonHeaders).
	
create_message_test() ->
	Store = mock_store:new({{read, unused}, {create, {{id, <<"message-id">>}, {rev, <<"rev">>}}} }),
	
	Message = #message{
		destination = "topic:sample.topic",
		body = <<"Sample Message Body">>,
		max_ttl = 300
	},
	
	#message{
		id = "message-id",
		destination = "topic:sample.topic",
		body = "Sample Message Body",
		max_ttl = 300,
		headers = []
	
	} = ?TEST_MOD:create(Store, Message).
	
create_message_with_headers_test() ->
	Store = mock_store:new({{read, unused}, {create, {{id, <<"message-id">>}, {rev, <<"rev">>}}} }),
	
	Message = #message{
		destination = "topic:sample.topic",
		body = <<"Sample Message Body">>,
		max_ttl = 300,
		headers = [{'Host', "www.sample.com"}, {'X-Powered-By', "Erlang"}]
	},
	
	#message{
		id = "message-id",
		destination = "topic:sample.topic",
		body = "Sample Message Body",
		max_ttl = 300,
		headers = [{'X-Powered-By', "Erlang"}]
	
	} = ?TEST_MOD:create(Store, Message).
	
create_batch_test() ->
	Store = mock_store:new({{read, unused}, {create, {{id, <<"batch-id">>}, {rev, <<"rev">>}}} }),
	
	Batch = #batch{
		destination = "topic:sample.topic",
		max_ttl = 300
	},
		
	#batch{
		id = "batch-id",
		destination = "topic:sample.topic",
		max_ttl = 300
	} = ?TEST_MOD:create_batch(Store, Batch).	
			