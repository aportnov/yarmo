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
	DestDocument = [
		{?l2b("_id"),  ?l2b("queue:existing.queue")},
		{?l2b("type"), ?l2b("queue")},
		{?l2b("name"), ?l2b("existing.queue")}
	],
	Store = mock_store:new([{read, DestDocument}]),
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
		headers = [{'Host', "www.sample-host.com"}, {'Message-Max-Ttl', "100"}, {'Message-Reply-Time', "40"}]
	},
	Store = mock_store:new([{read, not_found}, {create, {{id, "queue:nonexisting.queue"}, {rev, "rev"}}}]),
	Mod = handler_mod(Request, Store),
	
	{201, Headers, []} = Mod:handle(),
	[{'Location', _Loc}, {'Message-MAX-TTL', "100"}, {'Message-Reply-Time', "40"}] = Headers.

put_create_existing_destination_test() ->
	Request = #request{
		context_root = "queues", 
		method = 'PUT', 
		path = ["existing", "queue"],
		headers = [{'Host', "www.sample-host.com"}, {'Message-Max-Ttl', "100"}, {'Message-Reply-Time', "40"}]
	},
	DestDocument = [
		{?l2b("_id"),  ?l2b("queue:existing.queue")},
		{?l2b("type"), ?l2b("queue")},
		{?l2b("name"), ?l2b("existing.queue")}
	],

	Store = mock_store:new([{read, DestDocument}]),
	Mod = handler_mod(Request, Store),
	
	{204, [], []} = Mod:handle().

%% POST Create Message


%% POST Create Message Batch				
	
%% Helper Functions

handler_mod(#request{} = Request, Store) ->	
	yarmo_web_handler:new(Request, Store).