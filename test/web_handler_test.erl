-module(web_handler_test).
-author('author <alex.portnov@gmail.com>').

-include_lib("eunit/include/eunit.hrl").
-include("../src/yarmo.hrl").

%% GET Message 
existing_message_test() ->
	Request = #request{context_root = "queues", method = 'GET', path = ["sample", "queue", "messages", "message-id"]}, 
	
	JsonHeaders = [
		{struct,[{name, <<"X-H-One">>}, {value, <<"sample-one">>}]}, 
		{struct, [{name, <<"X-Powered-By">>}, {value, <<"Erlang">>}]}],
	
	Document = [
		{?l2b("_id"), ?l2b("message-id")},
		{?l2b("type"), ?l2b("message")},
		{?l2b("destination"), ?l2b("queue:destination") },
		{?l2b("max_ttl"), 222 },
		{?l2b("headers"), JsonHeaders },
		{?l2b("body"), <<"Sample Body">> },
		{?l2b("created_timestamp"), ?timestamp()}
	],

	{200, Headers, Body} = execute([{read, Document}], Request),
	
	Headers = [{'X-H-One', "sample-one"}, {'X-Powered-By', "Erlang"}],
	Body = "Sample Body".

message_not_found_test() ->
	Request = #request{context_root = "queues", method = 'GET', path = ["sample", "queue","messages", "message-id"]}, 
	{404, [], _} = execute([{read, not_found}], Request).

%% GET/HEAD Relationships
existing_queue_relationships_test() ->
	Request = #request{
		context_root = "queues", 
		method = 'HEAD', 
		path = ["existing", "queue"],
		headers = [{'Host', "www.sample-host.com"}]
	},

	{200, Headers, []} = execute([{read, mock_dest()}], Request),
	[{'Link', _Link}, {'Cache-Control', _CC}, {'Expires', _Expires}, {'ETag', _Tag}] = Headers.

nonexisting_queue_relationships_test() ->
	Request = #request{
		context_root = "queues", 
		method = 'HEAD', 
		path = ["nonexisting", "queue"],
		headers = [{'Host', "www.sample-host.com"}]
	},
	
	{404, [], _} = execute([{read, not_found}], Request).

%% PUT Create Destination
create_new_destination_test() ->
	Request = #request{
		context_root = "queues", 
		method = 'PUT', 
		path = ["nonexisting", "queue"],
		headers = [{'Host', "www.sample-host.com"}, {'Message-Max-Ttl', "100"}, {'Message-Reply-Time', "40"}, {'Message-Ack-Mode',"single"}]
	},
	
	MockStore = [{read, not_found}, {create, {{id, "queue:nonexisting.queue"}, {rev, "rev"}}}],
	{201, Headers, []} = execute(MockStore, Request),
	[{'Location', _Loc}, {'Message-Ack-Mode',"single"}, {'Message-MAX-TTL', "100"}, {'Message-Reply-Time', "40"}] = Headers.

create_existing_destination_test() ->
	Request = #request{
		context_root = "queues", 
		method = 'PUT', 
		path = ["existing", "queue"],
		headers = [{'Host', "www.sample-host.com"}, {'Message-Max-Ttl', "100"}, {'Message-Reply-Time', "40"}]
	},
	
	{204, [], []} = execute([{read, mock_dest()}], Request).

%% POST Create Message
create_message_test_() ->
	Request = #request{context_root = "queues", method = 'POST', 
		path = ["existing", "queue", "incoming"], params = [], headers = []},
	
	Assert = fun(Req, MsgId) ->
		CreateFun = case Req of
			#request{params = []} -> 
				fun([_Doc]) -> {{id, <<"message-id">>}, {rev, <<"Rev">>}} end;
			#request{params = [{"message_id", Id}]} ->
				fun([I, _Doc]) when I =:= Id -> {{id, ?l2b(Id)}, {rev, <<"Rev">>}} end
		end,	
		fun() ->
			{201, Headers, []} = execute([{read, mock_dest()},{create, CreateFun}], Req),
			[{'Location', "/queues/existing/queue/messages/" ++ MsgId}] = Headers			
		end	
	end,			
	
	[
		Assert(Request, "message-id"),
		Assert(Request#request{params = [{"message_id", "m-id"}]}, "m-id")
	].

%% POST Consume Message (Queue)

consume_message_from_queue_test_() ->
	Request = #request{context_root = "queues", method = 'POST', 
		path = ["existing", "queue", "poller"], params = [], headers = [{'Host', "www.some.com"}]},
		
	ViewFun = fun(["message", "undelivered", _]) ->
		Msg = [
			{?l2b("_id"), ?l2b("message-id")},
			{?l2b("_rev"), ?l2b("oldRev")},
			{?l2b("type"), ?l2b("message")},
			{?l2b("destination"), ?l2b("queue:destination") },
			{?l2b("max_ttl"), 222 },
			{?l2b("body"), <<"Sample Body">> },
			{?l2b("created_timestamp"), ?timestamp()}
		],
		[Msg]
	end,
	
	UpdateFun = fun(["message-id", "oldRev", _]) -> {{id, <<"message-id">>}, {rev, <<"newRev">>}} end,
	
	Assert = fun(AckMode, HeaderCheck) ->
		MockStore = [{read, mock_dest(AckMode)}, {view, ViewFun}, {update, UpdateFun}],
		fun() -> {200, Headers, "Sample Body" } = execute(MockStore, Request), HeaderCheck(Headers) end
	end,		
	
    [
		Assert("auto", fun(H) -> 
			[{'Content-Location', "http://www.some.com/queues/existing/queue/messages/message-id"}] = H 
		end),
		Assert("single", fun(H) -> 
			[{'Link', Link}, {'Content-Location', _}] = H,
			["http://www.some.com/queues/existing/queue/messages/message-id/acknowledgement", Tag, "rel=\"acknowledgement\""] = string:tokens(Link, "<>; "),
			["etag", _T] = string:tokens(Tag, "=")
		end)	
    ].					

consume_empty_queue_test() ->	
	Request = #request{context_root = "queues", method = 'POST', 
		path = ["existing", "queue", "poller"], params = [], headers = []},
		
	{503, [{'Retry-After', "5"}], <<"Service Unavailable">>} = execute([{read, mock_dest()},{view, []}], Request).

consume_bad_rev_test() ->
	Request = #request{context_root = "queues", method = 'POST', 
		path = ["existing", "queue", "poller"], params = [], headers = []},

	ViewFun = fun(["message", "undelivered", _]) ->
		Msg = [
			{?l2b("_id"), ?l2b("message-id")},
			{?l2b("_rev"), ?l2b("oldRev")},
			{?l2b("type"), ?l2b("message")},
			{?l2b("destination"), ?l2b("queue:destination") },
			{?l2b("max_ttl"), 222 },
			{?l2b("body"), <<"Sample Body">> },
			{?l2b("created_timestamp"), ?timestamp()}
		],
		[Msg]
	end,		

	MockStore = [{read, mock_dest()},{view, ViewFun}, {update, {{id, <<"message-id">>}, {rev, {bad_request, bad_rev}}}}],
	{400, [], <<"bad_rev">>} = execute(MockStore, Request).

consume_message_from_topic_test_() ->
	Request = #request{context_root = "topics", method = 'GET', params = [], headers = [{'Host', "www.some.com"}]},

    Timestamp = ?timestamp(),
	ViewFun = fun(["message", "undelivered", _]) ->
		Msg = [
			{?l2b("_id"), ?l2b("message-id")},
			{?l2b("_rev"), ?l2b("oldRev")},
			{?l2b("type"), ?l2b("message")},
			{?l2b("destination"), ?l2b("topic:existing.topic") },
			{?l2b("max_ttl"), 222 },
			{?l2b("body"), <<"Sample Body">> },
			{?l2b("created_timestamp"), Timestamp}
		],
		[Msg]
	end,
	
	AssertLinkHeader = fun(LinkHeader) ->
		[
			#link{href = "http://www.some.com/topics/existing/topic", rel = ["generator"]},
			#link{href = NextLink, rel = ["next"]}
		] = split_link(LinkHeader),
		[Tag, "next" | _BaseUrl] = lists:reverse(string:tokens(NextLink, "/:")),
		{"message-id", Timestamp} = yarmo_bin_util:decode(Tag)		
	end,
	
	Assert = fun(#request{} = Req) ->
		MockStore = [{read, mock_dest()}, {view, ViewFun}],
		fun() ->
			{200, Headers, "Sample Body" } = execute(MockStore, Req),
			[{'Link', Link}, {'Content-Location', _}] = Headers,
			AssertLinkHeader(Link)
	    end
	end,	
	[
		Assert(Request#request{path = ["existing", "topic", "poller", "last"]}),
		Assert(Request#request{path = ["existing", "topic", "poller", "first"]}),
		Assert(Request#request{path = ["existing", "topic", "poller", "next", "g2gCawANcHJpb3ItbWVzc2FnZW4FAK6K4sQO"]})
	].

consume_from_topic_no_message_test_() ->
	Request = #request{context_root = "topics", method = 'GET', params = [], headers = [{'Host', "www.some.com"}]},
    Timestamp = ?timestamp(),
	Msg = [
		{?l2b("_id"), ?l2b("message-id")},
		{?l2b("_rev"), ?l2b("oldRev")},
		{?l2b("type"), ?l2b("message")},
		{?l2b("destination"), ?l2b("topic:existing.topic") },
		{?l2b("max_ttl"), 222 },
		{?l2b("body"), <<"Sample Body">> },
		{?l2b("created_timestamp"), Timestamp}
	],

	Assert = fun(#request{} = Req, M) ->
		MockStore = [{read, mock_dest()}, {view, M}],
		fun() ->
			Response = execute(MockStore, Req),
			?assertEqual({503, [{'Retry-After', "5"}], <<"Service Unavailable">>}, Response)
		end
	end,
	
	[
		Assert(Request#request{path = ["existing", "topic", "poller", "last"]}, []),
		Assert(Request#request{path = ["existing", "topic", "poller", "first"]}, []),
		Assert(Request#request{path = ["existing", "topic", "poller", "next", "g2gCawANcHJpb3ItbWVzc2FnZW4FAK6K4sQO"]}, []),
		Assert(Request#request{path = ["existing", "topic", "poller", "next", yarmo_bin_util:encode({"message-id", Timestamp})]}, [Msg])
	].		

consume_from_topic_with_retry_test_() ->
	Request = #request{context_root = "topics", method = 'GET', params = [], headers = [{'Host', "www.some.com"}, {'Accept-Wait', "1"}]},
    Timestamp = ?timestamp(),
	Msg = [
		{?l2b("_id"), ?l2b("message-id")},
		{?l2b("_rev"), ?l2b("oldRev")},
		{?l2b("type"), ?l2b("message")},
		{?l2b("destination"), ?l2b("topic:existing.topic") },
		{?l2b("max_ttl"), 222 },
		{?l2b("body"), <<"Sample Body">> },
		{?l2b("created_timestamp"), Timestamp}
	],
	
	ViewFun = fun(["message", "undelivered", _]) ->
		case erlang:get(attempt) of 
			undefined -> erlang:put(attempt, 1), [];
			_         -> erlang:put(attempt, undefined), [Msg]
		end
	end,	
	Assert = fun(#request{} = Req) ->
		MockStore = [{read, mock_dest()}, {view, ViewFun}],
		fun() ->
			{200, Headers, "Sample Body" } = execute(MockStore, Req),
			[{'Link', _}, {'Content-Location', _}] = Headers
	    end
	end,	
	[
		Assert(Request#request{path = ["existing", "topic", "poller", "last"]}),
		Assert(Request#request{path = ["existing", "topic", "poller", "first"]}),
		Assert(Request#request{path = ["existing", "topic", "poller", "next", "g2gCawANcHJpb3ItbWVzc2FnZW4FAK6K4sQO"]})
	].
	
	
acknowledge_message_test_() ->
	Request = #request{context_root = "queues", method = 'POST', 
		path = ["existing", "queue", "messages", "message-id", "acknowledgement", "etag=8LPPTETYA9X636KIRE9L45L9E"], 
		params = [{acknowledgement, "true"}], headers = []},
	
	Document = [
			{?l2b("_id"), ?l2b("message-id")},
			{?l2b("_rev"), ?l2b("Rev")},
			{?l2b("type"), ?l2b("message")},
			{?l2b("destination"), ?l2b("queue:existing.queue") },
			{?l2b("max_ttl"), 222 },
			{?l2b("body"), <<"Sample Body">> },
			{?l2b("created_timestamp"), ?timestamp()}
	],
	
	Assert = fun(ExpectedResponse, Doc, Req) ->
		fun() -> ?assertEqual(ExpectedResponse, execute([{read, Doc}], Req)) end
	end,	
	[
		Assert({400, [], <<"acknowledgement parameter is required">>}, Document, Request#request{params = []}),
		Assert({404, [], <<"Not Found.">>}, not_found, Request),
		Assert({412, [], <<"Preconditions Failed">>}, [{?l2b("consumed_timestamp"), 1288} | Document], Request),
		Assert({204, [], []}, [{?l2b("consumed_timestamp"), 12345} | Document], Request),
		Assert({204, [], []}, [{?l2b("consumed_timestamp"), 12345} | Document], Request#request{params = [{acknowledgement, "false"}]})
	].	

%% POST Create Message Batch

create_batch_from_atom_feed_test() ->
	Request = #request{context_root = "queues", method = 'POST', 
		path = ["existing", "queue", "incoming", "batches"], 
		params = [], headers = [{'Content-Type', "application/atom+xml"}], body = mock_feed()},
	
	CreateFun = 
		fun(["urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a", _]) ->
			{{id, <<"urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a">>}, {rev, <<"MsgRev">>}};
		([_Doc]) -> {{id, <<"batch-id">>}, {rev, <<"BatchRev">>}}
	end,
	
	{201, [{'Content-Type', "text/uri-list"}, {'Location', Location}], Body } = execute([{read, mock_dest()}, {create, CreateFun}], Request),
	?assertEqual("/queues/existing/queue/batches/batch-id", Location),
	?assertEqual("/queues/existing/queue/messages/urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a", Body).  							

%% HEAD Retrieve POE URL
retrive_poe_message_url_test_() ->
	Request = #request{context_root = "queues", method = 'HEAD', 
		path = ["existing", "queue", "messages"], params = [], 
		headers = [{'POE', "11"}]},
		
	ReadFun =
		fun(["queue:existing.queue"]) -> (mock_dest())(["queue:existing.queue"]);
		([_Key]) -> fun([_K]) -> not_found end
	end,			

    CreateFun = fun([Doc]) ->
	    {value, {_, <<"poe-message">>}} = lists:keysearch(<<"type">>, 1, Doc),
		{{id, <<"poe-id">>}, {rev, <<"POERev">>}}
	end,    

    Assert = fun(ExpectedResponse, Req) ->
		fun() -> ?assertEqual(ExpectedResponse, execute([{read, ReadFun}, {create, CreateFun}], Req)) end
	end,
   
    [
		Assert({400, [], <<"POE header is required">>}, Request#request{headers = []}),
		Assert({200, [{'POE-Links', "/queues/existing/queue/messages/poe-id"}], []}, Request)
	]. 	

%% POST Create message with POE link
create_poe_message_test_() ->
	Request = #request{context_root = "queues", method = 'POST', 
		path = ["existing", "queue", "messages", "poe-id"], params = [], 
		headers = [{'POE', "11"}]},

	Document = [
		{<<"_id">>, <<"poe-id">>},
		{<<"_rev">>, <<"some">>},
		{<<"destination">>, <<"queue:existing.queue">>},
		{<<"poe">>, <<"11">>},
		{<<"max_ttl">>, 300},
		{<<"created_timestamp">>, 77777}
	],
	
	ReadFun = fun(["poe-id"]) -> Document;
				 (D) -> (mock_dest())(D)
	end,	

	Assert = fun(ExpectedResponse, MockStore, Req) ->
		fun() -> ?assertEqual(ExpectedResponse, execute(MockStore, Req)) end
	end,
	
	MockStore = fun(not_found) -> [{read, not_found}];
		           (Rev) -> [{read, ReadFun}, {update, {{id, <<"poe-id">>}, {rev, Rev}}}]
	end,	
	
	[
		Assert({404, [], <<"Not Found.">>}, MockStore(not_found), Request),
		Assert({412, [], <<"Preconditions Failed">>}, MockStore(refetch), Request),
		Assert({412, [], <<"Preconditions Failed">>}, MockStore([]), Request#request{headers = [{'POE', "10"}]}),
		Assert({400, [], <<"error">>}, MockStore({bad_request, error}), Request),
		Assert({204, [], []}, MockStore(<<"NewRev">>), Request)
	].	

%% POST create subscription
create_topic_subscription_test_() ->
	Request = #request{context_root = "topics", method = 'POST',
		path = ["existing", "topic", "subscribers"], params = [], headers = []
	},
	MockStore = [{read, mock_dest()}, {create, {{id, <<"sub-id">>}, {rev, <<"Rev">>}}}],
	
	Assert = fun(ExpectedResponse, Req) -> 
		fun() -> ?assertEqual(ExpectedResponse, execute(MockStore, Req)) end
	end,
	
	[
		Assert({400, [], <<"subscriber param is required.">>}, Request),
		Assert({201, [{'Location', "/topics/existing/topic/subscribers/sub-id"}], []},
		 	Request#request{params = [{"subscriber", "http://some-url"}]})
	].	

%% GET subscription
get_subscription_test_() ->
	Request = #request{context_root = "topics", method = 'GET', params = [], headers = []},
	Doc = [
	    {<<"_rev">>, <<"sub-rev">>},
		{<<"type">>, <<"subscription">>},
		{<<"subscriber">>, <<"http://www.some.com">>},
		{<<"poe">>, <<"false">>},
		{<<"created_timestamp">>, ?timestamp()}
	],
	Assert = fun(ExpectedResponse, Req) ->
		ReadFun = fun(["noise"]) -> not_found; 
			         (["sub-id"]) -> [{<<"destination">>, <<"topic:existing.topic">>}, {<<"_id">>, <<"sub-id">>} | Doc]; 
			         (["sub-nm"]) -> [{<<"destination">>, <<"topic:someother.topic">>}, {<<"_id">>, <<"sub-nm">>} | Doc]; 
			         (D) -> (mock_dest())(D) 
		end,
		MockStore = [{read, ReadFun}], 	
		fun() -> ?assertEqual(ExpectedResponse, execute(MockStore, Req)) end
	end,
	
	[
		Assert({404, [], <<"Not Found.">>}, Request#request{path = ["existing", "topic", "subscribers", "noise"]}),
		Assert({400, [], <<"Invalid Destination.">>}, Request#request{path = ["existing", "topic", "subscribers", "sub-nm"]}),
 		Assert({200, [{'Subscriber', "http://www.some.com"}, {'poe', "false"}], []}, Request#request{path = ["existing", "topic", "subscribers", "sub-id"]})
	].

%% DELETE subscription
unsubscribe_from_topic_test_() ->
	Request = #request{context_root = "topics", method = 'DELETE', params = [], headers = []},
	Doc = [
	    {<<"_rev">>, <<"sub-rev">>},
		{<<"type">>, <<"subscription">>},
		{<<"subscriber">>, <<"http://www.some.com">>},
		{<<"poe">>, <<"false">>},
		{<<"created_timestamp">>, ?timestamp()}
	],
	Assert = fun(ExpectedResponse, Req) ->
		ReadFun = fun(["noise"]) -> not_found; 
			         (["sub-id"]) -> [{<<"destination">>, <<"topic:existing.topic">>}, {<<"_id">>, <<"sub-id">>} | Doc]; 
			         (["sub-dl"]) -> [{<<"destination">>, <<"topic:existing.topic">>}, {<<"_id">>, <<"sub-dl">>} | Doc]; 
			         (["sub-nm"]) -> [{<<"destination">>, <<"topic:someother.topic">>}, {<<"_id">>, <<"sub-nm">>} | Doc]; 
			         (D) -> (mock_dest())(D) 
		end,
		DeleteFun = fun(["sub-id", _]) -> {ok, deleted};
			    	   (["sub-dl", _]) -> {error, some_error}	
		end, 	 
		MockStore = [{read, ReadFun}, {delete, DeleteFun}], 	
		fun() -> ?assertEqual(ExpectedResponse, execute(MockStore, Req)) end
	end,

	[
		Assert({204, [], []}, Request#request{context_root = "queues", path = ["existing", "queue", "subscribers", "sub-id"]}),
		Assert({204, [], []}, Request#request{path = ["existing", "topic", "subscribers", "noise"]}),
		Assert({204, [], []}, Request#request{path = ["existing", "topic", "subscribers", "sub-id"]}),
		Assert({400, [], <<"Invalid Destination.">>}, Request#request{path = ["existing", "topic", "subscribers", "sub-nm"]}),
		Assert({400, [], <<"some_error">>}, Request#request{path = ["existing", "topic", "subscribers", "sub-dl"]})
	].
				
	
%% Helper Functions
split_link(LinkHeader) ->
	Pred = fun(Link, Acc) ->                                                                                           
		[Href, "rel", Rel] = string:tokens(Link, "<>; \"="),
		[#link{href = Href, rel = [Rel]} | Acc]
	end,
	lists:foldr(Pred, [], string:tokens(LinkHeader, ",")).

execute(MockStore, Request) ->
	Store = mock_store:new(MockStore),

	Modules = [{message, yarmo_message}, {destination, yarmo_destination}],
	Options = lists:map(fun({Name, Mod}) -> {Name, Mod:new(Store)} end, Modules),
	try
		yarmo_config:start(Options),
		Mod = handler_mod(Request),
		Mod:handle()
	after
		yarmo_config:stop()
	end.	

mock_dest() ->
	mock_dest("auto").
	
mock_dest(AckMode) ->
	fun(["queue:existing.queue"]) ->
		[
			{?l2b("_id"),  ?l2b("queue:existing.queue")},
			{?l2b("type"), ?l2b("queue")},
			{?l2b("ack_mode"), ?l2b(AckMode)},
			{?l2b("name"), ?l2b("existing.queue")}
		];
	(["topic:existing.topic"]) ->
		[
			{?l2b("_id"),  ?l2b("topic:existing.topic")},
			{?l2b("type"), ?l2b("topic")},
			{?l2b("name"), ?l2b("existing.topic")}
		]
	end.
					

handler_mod(#request{} = Request) ->	
	yarmo_web_handler:new(Request).
	
mock_feed() ->
	"<?xml version='1.0' encoding='utf-8'?>
	<feed xmlns='http://www.w3.org/2005/Atom'>
	  <title>Example Feed</title>
	  <link href='http://example.org/'/>
	  <updated>2003-12-13T18:30:02Z</updated>
	  <id>urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6</id>
	  <entry>
	    <title>Atom-Powered Robots Run Amok 1</title>
	    <link href='http://example.org/2003/12/13/atom03'/>
	    <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
	    <updated>2003-12-13T18:30:02Z</updated>
	    <summary>Some text.</summary>
	  </entry>
	</feed>".	