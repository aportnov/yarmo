-module(yarmo_web_handler, [Request, Store]).
-author('author <alex.portnov@gmail.com>').

-export([handle/0]).

%% Export for testing
-export([link_header_builder/2, get_header/3, expires_header/2, make_etag/1]).

-include("yarmo.hrl").

%% Public API
handle() ->
	case Request#request.method of
		Method when Method =:= 'GET'; Method =:= 'HEAD' ->
			handle_get();
		'POST' ->
			handle_post();
		'PUT' ->
			handle_put();	
		_ ->
			{405, [], []}
	end.	

%% Request Handlers

handle_get()	->
	case lists:reverse(Request#request.path) of
		[MessageId, "messages" | _Destination ] when length(_Destination) > 0 ->
			get_message(MessageId);
		[BatchId, "batches" | Destination] ->
			get_batch(lists:reverse(Destination), BatchId);
		[] ->
			{404, [], []};
		Destination ->
			Dest = { ?l2a(Request#request.context_root), lists:reverse(Destination) },
			get_relationships(Dest)
	end.		

handle_post() ->	
	case lists:reverse(Request#request.path) of
	 	["incoming" | Destination] -> 
			Dest = { ?l2a(Request#request.context_root), lists:reverse(Destination) }, 
			post_message(Dest);
		["batches", "incomming" | Destination] ->
			Dest = { ?l2a(Request#request.context_root), lists:reverse(Destination) },
			post_batch(Dest); 
		_ -> {405, [], []}
	end.
	
handle_put() ->
	case lists:reverse(Request#request.path) of
		[] -> 
			{405, [], []};
		Destination when is_list(Destination) ->
			Dest = { ?l2a(Request#request.context_root), lists:reverse(Destination) }, 
			create_destination(Dest);
		_ -> 
			{501, [], []}

	end.	
	
%% Private API Implementation
get_message(MessageId) ->
	case yarmo_message:find(Store, MessageId) of
		not_found ->
			{404, [], []};
		Message ->
			{200, Message#message.headers, Message#message.body}	
	end.	
	
get_batch(_Destination, _Id) ->
	{501, [], []}.	
	
%% Relationships
get_relationships({topics, Topic}) ->
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
	get_relationships(Relationships, #destination{type = "topic", name = Topic});
	
get_relationships({queues, Queue}) ->
	Relationships = [
		{{rel, "post-message"}, {path, "incoming"}},
		{{rel, "post-batch"}, {path, "incoming/batches"}},
		{{rel, "post-message-once"}, {path, "messages"}},
		{{rel, "post-batch-once"}, {path, "batches"}},
		{{rel, "poller"}, {path, "poller"}}
	],	
	get_relationships(Relationships, #destination{type = "queue", name = Queue}).
	
get_relationships(Relationships, #destination{name = DestName} = Destination) ->
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
post_message({topics, Topic}) ->
	Destination = #destination{type = "topic", name = Topic},
	post_message(Destination, location_url(Request#request.context_root, Topic));

post_message({queues, Queue}) ->
	Destination = #destination{type = "queue", name = Queue},
	post_message(Destination, location_url(Request#request.context_root, Queue)).

post_message(#destination{} = Destination, MessageUrlFun) ->
	Dest = yarmo_destination:ensure_exist(Store, Destination),
	Msg = create_message(Dest, Request),
	{201, [{'Location', MessageUrlFun(message, Msg#message.id)}], []}.		

create_message(#destination{} = Dest, #request{} = Req) ->
	Document = #message {
		destination = Dest#destination.id, 
		max_ttl     = Dest#destination.max_ttl, 
		headers     = Req#request.headers,
		body        = Req#request.body
	}, 
	yarmo_message:create(Store, Document).

%% Message Batches	
post_batch({topics, Topic}) ->
	Destination = #destination{type = "topic", name = Topic},
	post_batch(Destination, location_url(Request#request.context_root, Topic));

post_batch({queues, Queue}) ->
	Destination = #destination{type = "queue", name = Queue},
	post_batch(Destination, location_url(Request#request.context_root, Queue)).

post_batch(#destination{} = Destination, UrlFun) ->
	Dest = yarmo_destination:ensure_exist(Store, Destination),

	Batch = yarmo_message:create_batch(Store, #batch{destination = Dest#destination.id, max_ttl = Dest#destination.max_ttl}),
	
	MsgFun = fun(#request{} = MsgReq, Acc) ->
		Msg = create_message(Dest, MsgReq),
		[UrlFun(message, Msg#message.id) | Acc]
	end,	
	Body = lists:foldr(MsgFun, [], parse_batch_body()),
	
	{201, [{'Content-Type', "text/uri-list"}, {'Location', UrlFun(batch, Batch#batch.id)}], string:join(Body, "\r\n") }.			

%% For now only parse multipart request. Need to implement atom feed parser.
parse_batch_body() ->
	#request{headers = Headers} = Request,
	case get_header('Content-Type', unknown, Headers) of
		unknown -> [];
		[$m,$u,$l,$t,$i,$p,$a,$r,$t,$/ | _] -> yarmo_web_multipart:parse_multipart_request(Request);
		_ -> []
	end.	

%% Destinations	
create_destination({topics, Topic}) ->
	create_destination(#destination{type = "topic", name = Topic});

create_destination({queues, Queue}) ->
	create_destination(#destination{type = "queue", name = Queue});

create_destination(#destination{} = Destination) ->
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
	Fun = fun(Key, Res) ->
		case lists:keysearch(Key, 1, Headers) of
			{value, {Key, Value}}  -> Value;
			_ -> Res	
		end
	end,	
	Convertion = if 
		is_atom(Name)   -> atom_to_list; 
		is_binary(Name) -> binary_to_list;
		true -> list_to_atom 
	end,
	
	case Fun(Name, not_found) of
		not_found -> Fun(erlang:Convertion(Name), Default);
		Value -> Value
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
	