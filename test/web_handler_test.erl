-module(web_handler_test).
-author('author <alex.portnov@gmail.com>').

-include_lib("eunit/include/eunit.hrl").
-include("../src/yarmo.hrl").

%% GET Message 
get_existing_message_test() ->
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
	Store = mock_store:new([{read, Document}]),
	
	Mod = handler_mod(Request, Store),
	{200, Headers, Body} = Mod:handle(),
	
	Headers = [{'X-H-One', "sample-one"}, {'X-Powered-By', "Erlang"}],
	Body = "Sample Body".

get_message_not_found_test() ->
	Request = #request{context_root = "queues", method = 'GET', path = ["sample", "queue","messages", "message-id"]}, 
	Store = mock_store:new([{read, not_found}]),
	
	Mod = handler_mod(Request, Store),
	{404, [], _} = Mod:handle().

%% GET/HEAD Relationships
get_existing_queue_relationships_test() ->
	Request = #request{
		context_root = "queues", 
		method = 'HEAD', 
		path = ["existing", "queue"],
		headers = [{'Host', "www.sample-host.com"}]
	},
	Store = mock_store:new([{read, mock_dest()}]),
	Mod = handler_mod(Request, Store),
	
	{200, Headers, []} = Mod:handle(),
	[{'Link', _Link}, {'Cache-Control', _CC}, {'Expires', _Expires}, {'ETag', _Tag}] = Headers.

get_nonexisting_queue_relationships_test() ->
	Request = #request{
		context_root = "queues", 
		method = 'HEAD', 
		path = ["nonexisting", "queue"],
		headers = [{'Host', "www.sample-host.com"}]
	},
	Store = mock_store:new([{read, not_found}]),
	Mod = handler_mod(Request, Store),
	
	{404, [], _} = Mod:handle().

%% PUT Create Destination
put_create_new_destination_test() ->
	Request = #request{
		context_root = "queues", 
		method = 'PUT', 
		path = ["nonexisting", "queue"],
		headers = [{'Host', "www.sample-host.com"}, {'Message-Max-Ttl', "100"}, {'Message-Reply-Time', "40"}, {'Message-Ack-Mode',"single"}]
	},
	Store = mock_store:new([{read, not_found}, {create, {{id, "queue:nonexisting.queue"}, {rev, "rev"}}}]),
	Mod = handler_mod(Request, Store),
	
	{201, Headers, []} = Mod:handle(),
	[{'Location', _Loc}, {'Message-Ack-Mode',"single"}, {'Message-MAX-TTL', "100"}, {'Message-Reply-Time', "40"}] = Headers.

put_create_existing_destination_test() ->
	Request = #request{
		context_root = "queues", 
		method = 'PUT', 
		path = ["existing", "queue"],
		headers = [{'Host', "www.sample-host.com"}, {'Message-Max-Ttl', "100"}, {'Message-Reply-Time', "40"}]
	},

	Store = mock_store:new([{read, mock_dest()}]),
	Mod = handler_mod(Request, Store),
	
	{204, [], []} = Mod:handle().

%% POST Create Message
post_create_message_test_() ->
	Request = #request{context_root = "queues", method = 'POST', 
		path = ["existing", "queue", "incoming"], params = [], headers = []},
	
	Assert = fun(CreateFun, Req, MsgId) ->
		fun() ->
			Store = mock_store:new([{read, mock_dest()},{create, CreateFun}]),
			Mod = handler_mod(Req, Store),
			{201, Headers, []} = Mod:handle(),
			[{'Location', "/queues/existing/queue/messages/" ++ MsgId}] = Headers			
		end	
	end,			
	
	[
		Assert(fun([_Doc]) -> {{id, <<"message-id">>}, {rev, <<"Rev">>}} end, Request, "message-id"),
		Assert(fun(["m-id", _Doc]) -> {{id, <<"m-id">>}, {rev, <<"Rev">>}} end, Request#request{params = [{"message_id", "m-id"}]}, "m-id")
	].

%% POST Consume Message (Queue)

post_consume_message_from_queue_test() ->
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
	
	UpdateFun = fun(["message-id", "oldRev", _]) -> {{id, <<"message-id">>}, {rev, <<"newRev">>}} end,

	Store = mock_store:new([{read, mock_dest()},{view, ViewFun}, {update, UpdateFun}]),
	Mod = handler_mod(Request, Store),
	{200, Headers, "Sample Body" } = Mod:handle(),
    [{'Content-Location', "/queues/existing/queue/messages/message-id"}] = Headers. 					

post_consume_empty_queue_test() ->	
	Request = #request{context_root = "queues", method = 'POST', 
		path = ["existing", "queue", "poller"], params = [], headers = []},
		
	Store = mock_store:new([{read, mock_dest()},{view, []}]),
	Mod = handler_mod(Request, Store),
	{503, [{'Retry-After', "5"}], <<"Service Unavailable">>} = Mod:handle().

post_consume_bad_rev_test() ->
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
	Store = mock_store:new([{read, mock_dest()},{view, ViewFun}, {update, {{id, <<"message-id">>}, {rev, {bad_request, bad_rev}}}}]),
	Mod = handler_mod(Request, Store),
	{400, [], <<"bad_rev">>} = Mod:handle().	

%% POST Create Message Batch				
	
%% Helper Functions

mock_dest() ->
	fun(["queue:existing.queue"]) ->
		[
			{?l2b("_id"),  ?l2b("queue:existing.queue")},
			{?l2b("type"), ?l2b("queue")},
			{?l2b("name"), ?l2b("existing.queue")}
		]
	end.			

handler_mod(#request{} = Request, Store) ->	
	yarmo_web_handler:new(Request, Store).