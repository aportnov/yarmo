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
	
message_to_doc_test_() ->
	Mod = test_mod(),
	
	Message = #message{
		destination = "topic:sample.topic",
		body = "Sample Message Body",
		max_ttl = 300,
		headers = [{'X-Powered-By', "Erlang"}]
	},
	
	Assert = fun(Msg, ShouldFind, ShouldNotFind) ->
		fun() ->
			L = Mod:message2doc(Msg),
			lists:map(fun(Name) -> N = ?l2b(Name), ?assertMatch({N, _}, proplists:lookup(N, L)) end, ShouldFind),
			lists:map(fun(Name) -> ?assertMatch(none, proplists:lookup(?l2b(Name), L)) end, ShouldNotFind)
		end	
	end,	
	
	[
		Assert(Message, ["destination", "body", "max_ttl", "headers"], ["created_timestamp", "_id", "_rev"]), 
		Assert(Message#message{created_timestamp = ?timestamp()}, 
			["destination", "body", "max_ttl", "headers", "created_timestamp"], ["_id", "_rev"]),
		Assert(Message#message{created_timestamp = ?timestamp(), id = "75757575"}, 
			["destination", "body", "max_ttl", "headers", "created_timestamp", "_id"], ["_rev"]),
		Assert(Message#message{id = "75757575", rev = "444"}, 
			["destination", "body", "max_ttl", "headers", "_id", "_rev"], ["created_timestamp"])
	].	
	
create_message_test() ->
	Mod = test_mod([{create, {{id, <<"message-id">>}, {rev, <<"rev">>}}}]),
	
	Message = #message{
		destination = "topic:sample.topic",
		body = <<"Sample Message Body">>,
		max_ttl = 300
	},
	
	#message{
		id = "message-id",
		rev = "rev",
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
		rev = "rev",
		destination = "topic:sample.topic",
		body = "Sample Message Body",
		max_ttl = 300,
		headers = [{'X-Powered-By', "Erlang"}]
	
	} = Mod:create(Message).

update_message_test_() ->
	Message = #message{id = "message-id", rev = "Rev", destination = "topic:sample.topic"},
	
	Assert = fun(Expected, UpdateOption) ->
		Mod = test_mod([{update, UpdateOption}]),
		fun() -> ?assertEqual(Expected, Mod:update(Message)) end
	end,			
	[
		Assert({ok, {rev, "NewRev"}}, {{id, <<"message-id">>}, {rev, <<"NewRev">>}}),
		Assert({conflict, refetch}, {{id, <<"message-id">>}, {rev, refetch}}),
		Assert({bad_request, format_error}, {{id, <<"message-id">>}, {rev, {bad_request, format_error}}})
	].	 	
	
create_batch_test() ->
	Mod = test_mod([{create, {{id, <<"batch-id">>}, {rev, <<"rev">>}}}]),
	
	Batch = #batch{destination = "topic:sample.topic", max_ttl = 300},
		
	#batch{
		id = "batch-id",
		rev = "rev",
		destination = "topic:sample.topic",
		max_ttl = 300
	} = Mod:create_batch(Batch).	

create_message_poe_test() ->
	CreateFun = fun([Doc]) ->
	  Val = fun(Name) ->
		case lists:keysearch(list_to_binary(Name), 1, Doc) of
			false -> [];
			{value, {_Name, Value}} -> Value
		end
	  end,	
		
	  ?assertEqual(<<"poe-message">>, Val("type")),
	  ?assertEqual(<<"queue:sample.queue">>, Val("destination")),	
	  ?assertEqual(<<"POE">>, Val("poe")),
	  ?assertEqual(1800, Val("max_ttl")),
	
	  {{id, <<"id">>}, {rev, <<"Rev">>}}	
	end	,
	
	Mod = test_mod([{create, CreateFun}]),
	Mod:create_poe_message(#destination{id = "queue:sample.queue", type = "queue"}, "POE").

update_message_poe_test_() ->
	Document = [
		{<<"_id">>, <<"poe-id">>},
		{<<"_rev">>, <<"some">>},
		{<<"poe">>, <<"POE">>},
		{<<"destination">>, <<"queue:sample.queue">>},
		{<<"max_ttl">>, 300},
		{<<"created_timestamp">>, 77777}
	],
	
	Assert = fun(ExpectedResponse, MockStore, POE) ->
		Mod = test_mod(MockStore),
		fun() ->
	    	?assertEqual(ExpectedResponse, Mod:update_poe_message(#message{id = "poe-id", destination = "queue:sample.queue"}, POE))
	    end 
	end,
	
	AssertUpdate = fun(ExpectedResponse, MockRev) ->
		MockStore = [{read, Document}, {update, {{id, <<"poe-id">>}, {rev, MockRev}}}],
		Assert(ExpectedResponse, MockStore, "POE")
	end,	
	
	[
		Assert(not_found, [{read, not_found}], "POE"),
		Assert({bad_request, poe_missmatch}, [{read, Document}], "WRONG-POE"),
		
		AssertUpdate({ok, {rev, "NewRev"}}, <<"NewRev">>),
		AssertUpdate({bad_request, error}, {bad_request, error}),
		AssertUpdate({conflict, refetch}, refetch)
	].

consume_message_test_() ->
	Document = [
		{<<"_id">>, <<"message-id">>},
		{<<"_rev">>, <<"some">>},
		{<<"destination">>, <<"queue:sample.queue">>},
		{<<"body">>, <<"Sample Message Body">>},
		{<<"max_ttl">>, 300},
		{<<"created_timestamp">>, 77777},
		{<<"headers">>, [{struct, [{name, <<"X-Powered-By">>}, {value, <<"Erlang">>}]}]}
	],
	
	Message = #message{
		id = "message-id",
		rev = "some",
		destination = "queue:sample.queue",
		body = "Sample Message Body",
		max_ttl = 300,
		headers = [{'X-Powered-By', "Erlang"}],
		created_timestamp = 77777
	},
	
	ViewFun = fun([DocName, ViewName, Options]) ->
		?assertEqual("message", DocName),
		?assertEqual("undelivered", ViewName),
		[{limit, 1}, {descending, true}, {startkey, _StartKey}, {endkey, EndKey}] = Options,
		?assertEqual("[\"queue:sample.queue\", 0]", EndKey),
		[Document]
	end,	
	
	Assert = fun(Expected, StoreMock, DestType) ->
		Mod = test_mod(StoreMock),
		fun() -> ?assertEqual(Expected, Mod:consume(#destination{id = "queue:sample.queue", type = atom_to_list(DestType)})) end
	end,
	[
		Assert(Message, [{view, ViewFun}], topic),
		Assert(Message#message{rev = "newRev"}, [{view, ViewFun}, {update, {{id, <<"message-id">>}, {rev, <<"newRev">>}}}], queue),
		Assert({error, bad_rev}, [{view, ViewFun}, {update, {{id, <<"message-id">>}, {rev, {bad_request, bad_rev}} }}], queue)
	].		

consume_retry_test() ->
	Document = [
		{<<"_id">>, <<"message-id">>},
		{<<"destination">>, <<"queue:sample.queue">>},
		{<<"body">>, <<"Sample Message Body">>},
		{<<"max_ttl">>, 300},
		{<<"created_timestamp">>, 77777},
		{<<"headers">>, [{struct, [{name, <<"X-Powered-By">>}, {value, <<"Erlang">>}]}]}
	],	
	
	ViewFun = fun([_DocName, _ViewName, _Options]) ->
		Rev = case erlang:get(rev) of undefined -> 1; Any -> Any end,
		erlang:put(rev, Rev + 1),
		[ [{<<"_rev">>, ?l2b(integer_to_list(Rev))} | Document] ]
	end,
	
	UpdateFun = fun([Key, OldRev, _Document]) ->
		case OldRev of
			"1" -> {{id, ?l2b(Key)}, {rev, refetch}};
			"2" -> {{id, ?l2b(Key)}, {rev, <<"3">>}}
		end
	end,	
	
	Mod = test_mod([{view, ViewFun}, {update, UpdateFun}]),		
	#message{
		id = "message-id",
		rev = "3",
		destination = "queue:sample.queue",
		body = "Sample Message Body",
		max_ttl = 300,
		headers = [{'X-Powered-By', "Erlang"}],
		created_timestamp = 77777
	} = Mod:consume(#destination{id = "queue:sample.queue", type = "queue"}).

next_topic_message_test_() ->
	Documents = [
	 [{<<"_id">>,<<"49b80429ed1688919e5cc9f274956b97">>},
	  {<<"_rev">>,<<"1-d9a1f58de407d6f777d43ca6333442ca">>},
	  {<<"type">>,<<"message">>},
	  {<<"destination">>,<<"topic:erlang.rest.sample.maxttl5">>},
	  {<<"max_ttl">>,1800},
	  {<<"body">>, <<>>},	
	  {<<"created_timestamp">>,63428067413}],
	 [{<<"_id">>,<<"6dfe8673604fc790f39bad41766b3e71">>},
	  {<<"_rev">>,<<"1-12d89ede0f16470746def1284f910619">>},
	  {<<"type">>,<<"message">>},
	  {<<"destination">>,<<"topic:erlang.rest.sample.maxttl5">>},
	  {<<"max_ttl">>,1800},
	  {<<"headers">>,
	   [{struct,[{<<"name">>,<<"Content-Type">>},
	             {<<"value">>,<<"image/jpg">>}]}]},
	  {<<"body">>,<<"230492304x0230942309x09213098234">>},
	  {<<"created_timestamp">>,63428067413}]
	],
	
	Mod = test_mod([{view, Documents}]),
	Assert = fun(ExpectedId, RequestedKey) ->
		fun() ->
			#message{id = Id} = Mod:next_message(#destination{id = "topic:erlang.rest.sample.maxttl5", type="topic"}, RequestedKey),
			?assertEqual(ExpectedId, Id)
		end
	end,
	[
		Assert("6dfe8673604fc790f39bad41766b3e71", {"49b80429ed1688919e5cc9f274956b97", 63428067413}),
		Assert("49b80429ed1688919e5cc9f274956b97", {"deleted-id", 63428067413})
	].		
	

acknowledge_message_test_() ->
	Document = [
		{<<"_id">>, <<"message-id">>},
		{<<"_rev">>, <<"some">>},
		{<<"destination">>, <<"queue:sample.queue">>},
		{<<"body">>, <<"Sample Message Body">>},
		{<<"max_ttl">>, 300},
		{<<"created_timestamp">>, 77777},
		{<<"headers">>, [{struct, [{name, <<"X-Powered-By">>}, {value, <<"Erlang">>}]}]}
	],	
	
	Assert = fun(Expected, StoreMock) ->
		Mod = test_mod(StoreMock),
		fun() -> ?assertEqual(Expected, Mod:acknowledge(#message{id = "message-id"})) end
	end,	
	[
		Assert(not_found, [{read, not_found}]),
		Assert({not_consumed, "some"}, [{read, [{<<"consumed_timestamp">>, <<>>} | Document]}]),
		Assert({acknowledged, "newRev"}, [{read, [{<<"consumed_timestamp">>, 11222} | Document]},
		 		{update, {{id, <<"message-id">>}, {rev, <<"newRev">>}}}]),
		Assert({error, bad_request}, [{read, [{<<"consumed_timestamp">>, 11222} | Document]},
		 		{update, {{id, <<"message-id">>}, {rev, {bad_request, bad_request}}}}])
	].		

test_mod() ->
	test_mod([]).

test_mod(StoreOptions) ->
	Store = mock_store:new(StoreOptions),
	yarmo_message:new(Store).			