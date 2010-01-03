-module(message_test).
-author('author <alex.portnov@gmail.com>').

-include_lib("eunit/include/eunit.hrl").

-include("../src/yarmo.hrl").

header_conversion_test() ->
	Headers = [{'Host', "www.sample.com"}, {'X-Powered-By', "Erlang"}],
	JsonHeaders = [{struct,[{name, <<"Host">>}, {value, <<"www.sample.com">>}]}, {struct, [{name, <<"X-Powered-By">>}, {value, <<"Erlang">>}]}],
	
	Mod = test_mod(),
 	JsonHeaders = Mod:headers2json(Headers),
	Headers     = Mod:json2headers(JsonHeaders).
	
create_message_test() ->
	Mod = test_mod([{create, {{id, <<"message-id">>}, {rev, <<"rev">>}}}]),
	
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
	
	} = Mod:create(Message).

create_message_with_headers_test() ->
	Mod = test_mod([{create, {{id, <<"message-id">>}, {rev, <<"rev">>}}}] ),
	
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
	
	} = Mod:create(Message).
	
create_batch_test() ->
	Mod = test_mod([{create, {{id, <<"batch-id">>}, {rev, <<"rev">>}}}]),
	
	Batch = #batch{
		destination = "topic:sample.topic",
		max_ttl = 300
	},
		
	#batch{
		id = "batch-id",
		destination = "topic:sample.topic",
		max_ttl = 300
	} = Mod:create_batch(Batch).	

consume_queue_message_test() ->
	Document = [
		{<<"_id">>, <<"message-id">>},
		{<<"_rev">>, <<"some">>},
		{<<"destination">>, <<"queue:sample.queue">>},
		{<<"body">>, <<"Sample Message Body">>},
		{<<"max_ttl">>, 300},
		{<<"headers">>, [{struct, [{name, <<"X-Powered-By">>}, {value, <<"Erlang">>}]}]}
	],
	
	ViewFun = fun([DocName, ViewName, Options]) ->
		?assertEqual("message", DocName),
		?assertEqual("undelivered", ViewName),
		[{limit, 1}, {descending, true}, {startkey, _StartKey}, {endkey, EndKey}] = Options,
		?assertEqual("[\"queue:sample.queue\", 0]", EndKey),
		[Document]
	end,	
	%% TODO test that update is being called
	
	Mod = test_mod([{view, ViewFun}]),
	
	#message{
		id = "message-id",
		destination = "queue:sample.queue",
		body = "Sample Message Body",
		max_ttl = 300,
		headers = [{'X-Powered-By', "Erlang"}]
	
	} = Mod:consume(#destination{id = "queue:sample.queue"}).	

test_mod() ->
	test_mod([]).

test_mod(StoreOptions) ->
	Store = mock_store:new(StoreOptions),
	yarmo_message:new(Store).			