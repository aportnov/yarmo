-module(yarmo_web_handler).
-author('author <alex.portnov@gmail.com>').

-behaviour(request_handler).

-export([handle/1]).

%% Export for testing
-export([link_header_builder/2, get_header/3, expires_header/2, make_etag/1]).

-include("yarmo.hrl").

-define(STORE, yarmo_store).

%% Public API
handle(#request{} = Request) ->
	case Request#request.method of
		Method when Method =:= 'GET'; Method =:= 'HEAD' ->
			handle_get(Request);
		'POST' ->
			handle_post(Request);
		'PUT' ->
			handle_put(Request);	
		_ ->
			{405, [], []}
	end;	
	
handle(_) ->
		 {501, [], []}.

%% Request Handlers

handle_get(Request)	->
	case lists:reverse(Request#request.path) of
		[MessageId, "messages" | _ ] ->
			get_message(MessageId, Request, ?STORE);
		[BatchId, "batches" | Destination] ->
			get_batch(lists:reverse(Destination), BatchId, Request);
		[] ->
			{404, [], []};
		Destination ->
			Dest = { ?l2a(Request#request.context_root), lists:reverse(Destination) },
			get_relationships(Dest, Request, ?STORE)
	end.		

handle_post(Request) ->	
	case lists:reverse(Request#request.path) of
	 	["incoming" | Destination] -> 
			Dest = { ?l2a(Request#request.context_root), lists:reverse(Destination) }, 
			post_message(Dest, Request, ?STORE);
		["batches", "incomming" | Destination] ->
			Dest = { ?l2a(Request#request.context_root), lists:reverse(Destination) },
			post_batch(Dest, Request, ?STORE); 
		_ -> {405, [], []}
	end.
	
handle_put(Request) ->
	case lists:reverse(Request#request.path) of
		[] -> 
			{405, [], []};
		Destination when is_list(Destination) ->
			Dest = { ?l2a(Request#request.context_root), lists:reverse(Destination) }, 
			create_destination(Dest, Request, ?STORE);
		_ -> 
			{501, [], []}

	end.	
	
%% Private API Implementation
get_message(MessageId, _Request, Store) ->
	case yarmo_message:find(Store, MessageId) of
		not_found ->
			{404, [], []};
		Message ->
			{200, Message#message.headers, Message#message.body}	
	end.	
	
get_batch(_Destination, _Id, _Request) ->
	{501, [], []}.	
	
%% Relationships
get_relationships({topics, Topic}, Request, Store) ->
	Relationships = [
		{{rel, "post-message"}, {path, "incoming"}},
		{{rel, "post-batch"}, {path, "incoming/batches"}},
		{{rel, "post-message-once"}, {path, "messages"}},
		{{rel, "post-batch-once"}, {path, "batches"}},
		{{rel, "first"}, {path, "poller/first"}},
		{{rel, "last"}, {path, "poller/last"}},
		{{rel, "first-batch"}, {path, "poller/batches/first"}},
		{{rel, "last-batch"}, {path, "poller/batches/last"}}
	],	
	get_relationships(Relationships, #destination{type = "topic", name = Topic}, Request, Store);
	
get_relationships({queues, Queue}, Request, Store) ->
	Relationships = [
		{{rel, "post-message"}, {path, "incoming"}},
		{{rel, "post-batch"}, {path, "incoming/batches"}},
		{{rel, "post-message-once"}, {path, "messages"}},
		{{rel, "post-batch-once"}, {path, "batches"}},
		{{rel, "poller"}, {path, "poller"}}
	],	
	get_relationships(Relationships, #destination{type = "queue", name = Queue}, Request, Store).
	
get_relationships(Relationships, #destination{name = DestName} = Destination, Request, Store) ->
	case yarmo_destination:find(Store, Destination) of
		not_found ->
			{404, [], []};
		_Dest ->	
			HostHeader = get_header('Host', [], Request#request.headers),

			Builder = link_header_builder(Relationships, Request#request.context_root),	
			LinkHeader = Builder(DestName, HostHeader),
			Headers = [
				LinkHeader,
				{'Cache-Control', "public, max-age=\"86400\""},
				{'Expires', expires_header(erlang:universaltime(), 86400)},
				{'ETag', make_etag(LinkHeader)}
			],
			{200, Headers, []}
	end.	
			
%% Messages
post_message({topics, Topic}, Request, Store) ->
	Destination = #destination{type = "topic", name = Topic},
	post_message(Destination, location_url(Request#request.context_root, Topic), Request, Store);

post_message({queues, Queue}, Request, Store) ->
	Destination = #destination{type = "queue", name = Queue},
	post_message(Destination, location_url(Request#request.context_root, Queue), Request, Store).

post_message(#destination{} = Destination, MessageUrlFun, Request, Store) ->
	Dest = yarmo_destination:ensure_exist(Store, Destination),
	Msg = create_message(Dest, Request, Store),
	{201, [{'Location', MessageUrlFun(message, Msg#message.id)}], []}.		

create_message(#destination{} = Dest, Request, Store) ->
	Document = #message {
		destination = Dest#destination.id, 
		max_ttl     = Dest#destination.max_ttl, 
		headers     = Request#request.headers,
		body        = Request#request.body
	}, 
	yarmo_message:create(Store, Document).

%% Message Batches	
post_batch({topics, Topic}, Request, Store) ->
	Destination = #destination{type = "topic", name = Topic},
	post_batch(Destination, location_url(Request#request.context_root, Topic), Request, Store);

post_batch({queues, Queue}, Request, Store) ->
	Destination = #destination{type = "queue", name = Queue},
	post_batch(Destination, location_url(Request#request.context_root, Queue), Request, Store).

post_batch(#destination{} = Destination, UrlFun, Request, Store) ->
	Dest = yarmo_destination:ensure_exist(Store, Destination),

	Batch = yarmo_message:create_batch(Store, #batch{destination = Dest#destination.id, max_ttl = Dest#destination.max_ttl}),
	
	MsgFun = fun(#request{} = MsgReq, Acc) ->
		Msg = create_message(Dest, MsgReq, Store),
		[UrlFun(message, Msg#message.id) | Acc]
	end,	
	Body = lists:foldr(MsgFun, [], parse_batch_body(Request)),
	
	{201, [{'Content-Type', "text/uri-list"}, {'Location', UrlFun(batch, Batch#batch.id)}], string:join(Body, "\r\n") }.			

%% For now only parse multipart request. Need to implement atom feed parser.
parse_batch_body(#request{headers = Headers} = Request) ->
	case get_header('Content-Type', unknown, Headers) of
		unknown -> [];
		[$m,$u,$l,$t,$i,$p,$a,$r,$t,$/ | _] -> yarmo_web_multipart:parse_multipart_request(Request);
		_ -> []
	end.	

%% Destinations	
create_destination({topics, Topic}, Request, Store) ->
	create_destination(#destination{type = "topic", name = Topic}, Request, Store);

create_destination({queues, Queue}, Request, Store) ->
	create_destination(#destination{type = "queue", name = Queue}, Request, Store);

create_destination(#destination{} = Destination, Request, Store) ->
	Header = fun(Name, Default) -> 
		Value = integer_to_list(Default),
		list_to_integer(get_header(Name, Value, Request#request.headers)) 
	end,
	
	MaxTtl = Header("Message-Max-Ttl", Destination#destination.max_ttl),
	ReplyTime = Header("Message-Reply-Time", Destination#destination.reply_time),

	case yarmo_destination:find(Store, Destination) of
		not_found ->
			Dest = yarmo_destination:create(Store, Destination#destination{max_ttl = MaxTtl, reply_time = ReplyTime}),
			Location = destination_url(Request#request.context_root, Destination#destination.name),
			{201, [ {'Location', Location}, 
					{'Message-MAX-TTL', integer_to_list(Dest#destination.max_ttl)},
					{'Message-Reply-Time', integer_to_list(Dest#destination.reply_time)} ], []};
		_Dest ->
			{204, [], []}
	end.		

%% Utility Functions

link_header_builder(Relationships, ContextRoot) ->
	fun(Destination, Host) ->
		BasePath = [ContextRoot | Destination],

		Fun = fun({{rel, Rel}, {path, Suffix}}, Acc) ->
			Path = "/" ++ string:join(BasePath ++ [Suffix], "/"),
			Link = "<http://" ++ Host ++ Path ++ ">; rel=\"" ++ Rel ++ "\"",
			[Link | Acc]
		end,
		Link = string:join(lists:foldr(Fun, [], Relationships), ", "),
		
		{'Link', Link}
	end.	

	
get_header(Name, Default, Headers) ->
	case lists:keysearch(Name, 1, Headers) of
		{value, {Name, Value}}  -> Value;
		_ -> Default	
	end.

location_url(ContextRoot, Destination) ->
	fun(Type, Id) ->
		Suffix = case Type of
			message -> "/messages/";
			batch   -> "/batches/";
			_       -> "/"
		end,
		destination_url(ContextRoot, Destination) ++ Suffix ++ Id	
	end.	
	
destination_url(ContextRoot, Destination) ->
	"/" ++ ContextRoot ++ "/" ++ string:join(Destination, "/").		
	
expires_header(DateTime, TtlSeconds) ->
	Sec = calendar:datetime_to_gregorian_seconds(DateTime),
	ExpireDatetime = calendar:gregorian_seconds_to_datetime(Sec + TtlSeconds),
	httpd_util:rfc1123_date(ExpireDatetime).

make_etag(Term) ->
    <<SigInt:128/integer>> = erlang:md5(term_to_binary(Term)),
    "\"" ++ lists:flatten(io_lib:format("~.36B",[SigInt])) ++ "\"".
	