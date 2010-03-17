-module(yarmo_web_handler, [Request]).
-author('author <alex.portnov@gmail.com>').

-export([handle/0]).

-include("yarmo.hrl").

-define(ETAG(Term), yarmo_bin_util:etag(Term)).
-define(MD5(Term), yarmo_bin_util:md5(Term, 36)).
-define(ENCODE(Term), yarmo_bin_util:encode(Term)).
-define(DECODE(Data), yarmo_bin_util:decode(Data)).

-define(LINK(Links), yarmo_link_util:link_header(Links)).
-define(OPTION(Name, Default, Options), yarmo_web_util:get_option(Name, Default, Options)).

-define(MSG_MOD, yarmo_config:get(message)).
-define(DEST_MOD, yarmo_config:get(destination)).

%% Public API
handle() ->
	case Request#request.method of
		Method when Method =:= 'GET'; Method =:= 'HEAD' ->
			handle_get();
		'POST' ->
			handle_post();
		'PUT' ->
			handle_put();	
		'DELETE' ->
			handle_delete();	
		_ ->
			{405, [], <<"Method Not Allowed.">>}
	end.	

%% Request Handlers

handle_get() ->
	case lists:reverse(Request#request.path) of
		[MessageId, "messages" | _Destination ] when length(_Destination) > 0 ->
			get_message(MessageId);
		["messages" | Destination] ->
			poe_request(lists:reverse(Destination), fun get_poe_url/2);	
		["last", "poller" | Destination] ->
			with_destination(lists:reverse(Destination), fun last_message/1);			
		["first", "poller" | Destination] ->
			with_destination(lists:reverse(Destination), fun first_message/1);	
		[Bookmark, "next", "poller" | Destination] ->
			with_destination(lists:reverse(Destination), fun(D) -> next_message(D, Bookmark) end);				
		[BatchId, "batches" | Destination] ->
			get_batch(lists:reverse(Destination), BatchId);	
		[SubscriptionId, "subscribers" | Destination] ->
			with_destination(lists:reverse(Destination), fun(D) -> get_subscription(D, SubscriptionId) end);	
		[] ->
			{404, [], <<"Not Found.">>};
		Destination ->
			Dest = name_to_destination(lists:reverse(Destination)),
			with_destination(Dest#destination.name, fun get_relationships/1)
	end.		

handle_post() ->	
	case lists:reverse(Request#request.path) of
	 	["incoming" | Destination] -> 
			with_destination(lists:reverse(Destination), fun post_message/1);
		[MessageId, "messages" | Destination] ->
			poe_request(lists:reverse(Destination), fun(D, POE) -> post_poe_message(D, POE, MessageId) end);
		["batches", "incoming" | Destination] ->
			with_destination(lists:reverse(Destination), fun post_batch/1);
		["poller" | Destination] ->
			with_destination(lists:reverse(Destination), fun consume_message/1);		
		["subscribers" | Destination] when Request#request.context_root =:= "topics" ->
			with_destination(lists:reverse(Destination), fun create_subscription/1);		
		[AckTag, "acknowledgement", MessageId, "messages" | _Destination] when length(_Destination) > 0 ->
			["etag", Tag] = string:tokens(AckTag, "="),
		    acknowledge_message(MessageId, Tag);			
		_ -> {405, [], <<"Method Not Allowed.">>}
	end.
	
handle_put() ->
	case lists:reverse(Request#request.path) of
		[] -> 
			{405, [], <<"Method Not Allowed.">>};
		Destination when is_list(Destination) ->
			FoundCallback = fun(_D) -> {204, [], []} end,	
			NotFoundCallback = fun create_destination/1,
			with_destination(lists:reverse(Destination), FoundCallback, NotFoundCallback);
		_ -> 
			{501, [], <<"Not Implemented.">>}

	end.	

handle_delete() ->
	case lists:reverse(Request#request.path) of
		[SubscriptionId, "subscribers" | Destination] ->
			with_destination(lists:reverse(Destination), fun(D) -> delete_subscription(D, SubscriptionId) end);
		_ ->
			{501, [], <<"Not Implemented.">>}		
	end.	
	
%% Private API Implementation
get_message(MessageId) ->
	Mod = ?MSG_MOD,
	case Mod:find(MessageId) of
		not_found ->
			{404, [], <<"Not Found.">>};
		Message ->
			{200, Message#message.headers, Message#message.body}	
	end.	
	
get_batch(_Destination, _Id) ->
	{501, [], <<"Not Implemented.">>}.	
	
%% Relationships
get_relationships(#destination{type = "topic"} = Destination) ->
	Relationships = [
		{{rel, "post-message"}, {path, "incoming"}},
		{{rel, "post-batch"}, {path, "incoming/batches"}},
		{{rel, "post-message-once"}, {path, "messages"}},
		{{rel, "post-batch-once"}, {path, "batches"}},
		{{rel, "first"}, {path, "poller/first"}},
		{{rel, "last"}, {path, "poller/last"}},
		{{rel, "first-batch"}, {path, "poller/batches/first"}},
		{{rel, "last-batch"}, {path, "poller/batches/last"}},
		{{rel, "subscribers"}, {path, "subscribers"}}
	],	
	get_relationships(Relationships, Destination);
	
get_relationships(#destination{type = "queue"} = Destination) ->
	Relationships = [
		{{rel, "post-message"}, {path, "incoming"}},
		{{rel, "post-batch"}, {path, "incoming/batches"}},
		{{rel, "post-message-once"}, {path, "messages"}},
		{{rel, "post-batch-once"}, {path, "batches"}},
		{{rel, "poller"}, {path, "poller"}}
	],	
	get_relationships(Relationships, Destination).
	
get_relationships(Relationships, #destination{name = DestName}) ->
	Fun = fun({{rel, Rel}, {path, Path}}, Acc) ->
		Href = full_destination_url("http", DestName, Path),
		[#link{rel = [Rel], href =  Href} | Acc]
	end,	
	Links = lists:foldr(Fun, [], Relationships),

	LinkHeader = ?LINK(Links),
	Headers = [
	    LinkHeader,
		{'Cache-Control', "public, max-age=\"86400\""},
		{'Expires', yarmo_web_util:expires(erlang:universaltime(), 86400)},
		{'ETag', ?ETAG(LinkHeader)}
	],
	{200, Headers, []}.
			
%% Messages
post_message(#destination{name = Name} = Destination) ->
	MessageUrlFun = location_url(Name),
	MsgMod = ?MSG_MOD,
	
	Msg = create_message(Destination, Request, fun(Msg) -> MsgMod:create(Msg) end),
	push_message(Destination, Msg),
	
	{201, [{'Location', MessageUrlFun(message, Msg#message.id)}], []}.		

create_message(#destination{id = DestId, max_ttl = MaxTtl}, #request{headers = Headers, body = Body, params = Params}, CreateFun) ->
	Document = #message {
		destination = DestId, 
		max_ttl     = MaxTtl, 
		headers     = Headers,
		body        = Body,
		id          = ?OPTION('message_id', generated, Params)
	}, 
	CreateFun(Document).

consume_message(#destination{name = Name, type = "queue", ack_mode = AckMode} = Destination) ->
    MsgMod = ?MSG_MOD,

	LinkFun = fun(P, Rel) -> #link{href= full_destination_url("http", Name, P), rel = [Rel]} end,	

	ResponseFun = fun(#message{id = MsgId, consumed_timestamp = Timestamp, headers = Headers} = Message) ->
		case AckMode of
			"single" ->
				ETag = ?MD5({consumed_timestamp, Timestamp}),
				Path = string:join(["messages", MsgId, "acknowledgement;etag=" ++ ETag], "/"),
				
				{200, [?LINK([LinkFun(Path, "acknowledgement")]) | Headers], Message#message.body};
			_  ->
				spawn(fun() -> catch MsgMod:acknowledge(Message) end),
				{200, Headers, Message#message.body}					
		end				
	end,
	
	case MsgMod:consume(Destination) of
	  not_found -> 
		{503, [{'Retry-After', "5"}], <<"Service Unavailable">>};			
	  {error, Error} -> 
		{400, [], yarmo_bin_util:thing_to_bin(Error)};				
	  #message{} = Message -> 
		Msg = add_content_location(Destination, Message),
		ResponseFun(Msg)		
	end.	

retrieve_message(#destination{name = Name, type = "topic"} = Destination, RetrieveFun) ->
	LinkFun = fun(P, Rel) -> #link{href= full_destination_url("http", Name, P), rel = [Rel]} end,	

	RetrieveMsgFun = case ?OPTION('Accept-Wait', none, Request#request.headers) of
		none  -> RetrieveFun;
		Value ->
			fun() -> 
				case RetrieveFun() of
					not_found ->
						Timeout = lists:min([5, list_to_integer(Value)]),
						timer:sleep(Timeout * 1000),
						RetrieveFun();
					Any -> Any	
				end	
			end	
	end,	
		
	case RetrieveMsgFun() of
	  not_found -> 
		{503, [{'Retry-After', "5"}], <<"Service Unavailable">>};			
	  #message{id = Id, created_timestamp = Timestamp} = Message -> 
		#message{headers = Headers} = add_content_location(Destination, Message),
		Path = string:join(["poller", "next", ?ENCODE({Id, Timestamp})], "/"),

		{200, [?LINK([LinkFun([], "generator"), LinkFun(Path, "next")]) | Headers], Message#message.body}
	end.	

add_content_location(#destination{name = Name}, #message{id = Id} = Msg) ->
	MessageUrlFun = location_url(Name),
	Headers = [{'Content-Location', MessageUrlFun(message, Id)} | Msg#message.headers],
	Msg#message{headers = Headers}.			
			
last_message(#destination{type = "topic"} = Destination) ->
    MsgMod = ?MSG_MOD,
	retrieve_message(Destination, fun() -> MsgMod:consume(Destination) end).

first_message(#destination{type = "topic"} = Destination) ->
    MsgMod = ?MSG_MOD,
	retrieve_message(Destination, fun() -> MsgMod:first_message(Destination) end).

next_message(#destination{type = "topic"} = Destination, Bookmark) ->	
	MsgMod = ?MSG_MOD,
	retrieve_message(Destination, fun() -> MsgMod:next_message(Destination, ?DECODE(Bookmark)) end).

acknowledge_message(MessageId, ETag) ->
    MsgMod = ?MSG_MOD,
	
	case ?OPTION('acknowledgement', [], Request#request.params) of
		[]  -> {400, [], <<"acknowledgement parameter is required">>};
		Ack ->
			case MsgMod:find(MessageId) of
			  not_found -> {404, [], <<"Not Found.">>};
			  #message{consumed_timestamp = Timestamp} = Message ->
				case ?MD5({consumed_timestamp, Timestamp}) of
					ETag when Ack =:= "true" ->
				  		spawn(fun() -> catch MsgMod:acknowledge(Message) end),
						{204, [], []};
				  	ETag -> 
						spawn(fun() -> catch MsgMod:update(Message#message{consumed_timestamp = []}) end),
						{204, [], []};
				  	_ -> {412, [], <<"Preconditions Failed">>}		
				end	 
			end			
	end.			
		
get_poe_url(#destination{name = Name} = Destination, POE) ->
	MsgMod = ?MSG_MOD,

	{id, MsgId} = MsgMod:create_poe_message(Destination, POE),

	Link = full_destination_url("http", Name, string:join(["messages", MsgId], "/")),
	{200, [{'POE-Links', Link}], []}.

post_poe_message(#destination{} = Destination, POE, MessageId) ->
	MsgMod = ?MSG_MOD,
    ?LOG("D", [Destination]),
	Fun = fun(Msg) -> 
		case MsgMod:update_poe_message(Msg, POE) of
			{ok, {rev, _}} = Res ->
				push_message(Destination, Msg), Res;
			Any -> Any	
		end	
	end,
	
	case create_message(Destination, Request#request{params = [{"message_id", MessageId}]}, Fun) of
		not_found                    -> {404, [], <<"Not Found.">>};
		{conflict, refetch}          -> {412, [], <<"Preconditions Failed">>}; 
		{bad_request, poe_missmatch} -> {412, [], <<"Preconditions Failed">>};
		{bad_request, Error}         -> {400, [], yarmo_bin_util:thing_to_bin(Error)};
		{ok, {rev, _Rev}}            -> {204, [], []}
	end.		
	
%% Message Batches
%% TODO - I should change order here. Should create a batch, save batch ID with every message. 
post_batch(#destination{name = Name} = Destination) ->
	UrlFun = location_url(Name),
	MsgMod = ?MSG_MOD,
	
	MsgFun = fun(#request{} = MsgReq, Acc) ->
		Msg = create_message(Destination, MsgReq, fun(Doc) -> MsgMod:create(Doc) end),
		push_message(Destination, Msg),
		[UrlFun(message, Msg#message.id) | Acc]
	end,	
	Body = lists:foldr(MsgFun, [], parse_batch_body()),

	Batch = MsgMod:create_batch(#batch{destination = Destination#destination.id, max_ttl = Destination#destination.max_ttl, body = Body}),
	
	{201, [{'Content-Type', "text/uri-list"}, {'Location', UrlFun(batch, Batch#batch.id)}], string:join(Body, "\r\n") }.			

parse_batch_body() ->
	#request{headers = Headers} = Request,
	case ?OPTION('Content-Type', unknown, Headers) of
		unknown -> [];
		[$m,$u,$l,$t,$i,$p,$a,$r,$t,$/ | _] -> yarmo_web_multipart:parse_multipart_request(Request);
		"application/atom+xml" -> yarmo_web_atom:parse_atom_request(Request);
		Any -> 
			?LOG("Unhandled Content:", [Any]), []
	end.	

%% Destinations	
create_destination(#destination{} = Destination) ->
	Header = fun(Name, Default) -> 
		case Default of
			D when is_integer(D) ->
				list_to_integer(?OPTION(Name, integer_to_list(D), Request#request.headers));
			L ->
				?OPTION(Name, L, Request#request.headers)
		end	
	end,
	
	MaxTtl = Header("Message-Max-Ttl", Destination#destination.max_ttl),
	ReplyTime = Header("Message-Reply-Time", Destination#destination.reply_time),
	AckMode = Header("Message-Ack-Mode", Destination#destination.ack_mode),
	
	Mod = ?DEST_MOD,
	Dest = Mod:create(Destination#destination{max_ttl = MaxTtl, reply_time = ReplyTime, ack_mode = AckMode}),

	{201, [ {'Location', destination_url(Destination#destination.name)}, 
			{'Message-Ack-Mode', Dest#destination.ack_mode},
			{'Message-MAX-TTL', integer_to_list(Dest#destination.max_ttl)},
			{'Message-Reply-Time', integer_to_list(Dest#destination.reply_time)} ], []}.

get_subscription(#destination{id = DestId}, SubscriptionId) ->
	Mod = ?DEST_MOD,
	case Mod:subscriber(SubscriptionId) of
		not_found -> 
			{404, [], <<"Not Found.">>};
		#subscription{destination =  DestId, poe = POE, subscriber = Subscriber} ->
			{200, [{'Subscriber', Subscriber}, {'poe', POE}], []};
		_ -> 
			{400, [], <<"Invalid Destination.">>}	
	end.	

%% Create a subscriber for PUSH delivery
create_subscription(#destination{type = "queue"}) ->
	{412, [], <<"Subscriptions only supported by topics">>};
	
create_subscription(#destination{name = Name, type = "topic"} = Destination) ->
	Mod = ?DEST_MOD,
	case ?OPTION('subscriber', undefined, Request#request.params) of
		undefined  ->
			{400, [], <<"subscriber param is required.">>};
		Subscriber ->
			POE = ?OPTION('POE', "false", Request#request.params),
			#subscription{id = Id} = Mod:subscribe(Destination, Subscriber, POE),
			UrlFun = location_url(Name),
			{201, [{'Location', UrlFun(subscription, Id)}], []}
	end.			

%% Delete topic subscription
delete_subscription(#destination{type = "queue"}, _) -> {204, [], []};
delete_subscription(#destination{id = DestId, type = "topic"}, SubscriptionId) ->
	Mod = ?DEST_MOD,
	case Mod:subscriber(SubscriptionId) of
		#subscription{destination =  DestId} = Sub ->
			case Mod:unsubscribe(Sub) of
				{ok, _}        -> {204, [], []};
				{error, Error} -> {400, [], yarmo_bin_util:thing_to_bin(Error)}
			end;	
		not_found -> {204, [], []};
		_         -> {400, [], <<"Invalid Destination.">>}	
	end.	

%% Filters
with_destination(Name, FoundCallback) ->
	with_destination(Name, FoundCallback, fun(_D) -> {404, [], <<"Not Found.">>} end).

with_destination(Name, FoundCallback, NotFoundCallback) ->
	Destination = name_to_destination(Name),
	Mod = ?DEST_MOD,
	
	case Mod:find(Destination) of
		not_found -> NotFoundCallback(Destination);
		Dest      -> FoundCallback(Dest)
	end.

poe_request(Destination, Callback) ->
	Fun = fun(D) ->
		case ?OPTION('POE', [], Request#request.headers) of
			[]  -> {400, [], <<"POE header is required">>};
			POE -> Callback(D, POE)
		end		
	end,	
	with_destination(Destination, Fun).
	
%% Utility Functions
push_message(#destination{type = "topic"}, #message{} = Msg) ->
	Mod = ?DEST_MOD,
	spawn(fun() -> Mod:deliver(Msg, fun ibrowse:send_req/4) end),
	{ok, spawn};

push_message(#destination{type = "queue"}, _Msg) ->
	{ok, no_action}.

name_to_destination(Name) ->
	Type = case ?l2a(Request#request.context_root) of
		queues -> "queue";
		topics -> "topic"
	end,
	#destination{type = Type, name = Name}.
	
location_url(Destination) ->
	fun(Type, Id) ->
		Suffix = case Type of
			message      -> "messages/";
			batch        -> "batches/";
			subscription -> "subscribers/";
			_            -> ""
		end,
		full_destination_url("http", Destination, Suffix ++ Id)	
	end.	
	
destination_url(Destination) ->
	"/" ++ Request#request.context_root ++ "/" ++ string:join(Destination, "/").	

full_destination_url(Scheme, DestName, Path) ->
	ReqPath = case Path of
		[] -> destination_url(DestName);
		_  -> destination_url(DestName) ++ "/" ++ Path
	end,	
	case ?OPTION('Host', [], Request#request.headers) of
		[]   -> ReqPath;
		Host -> Scheme ++ "://" ++ Host ++ ReqPath
	end.			