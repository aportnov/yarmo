-module(yarmo_web_handler, [Request, Store]).
-author('author <alex.portnov@gmail.com>').

-export([handle/0]).

%% Export for testing
-export([get_option/3, expires_header/2, make_etag/1]).

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
			{405, [], <<"Method Not Allowed.">>}
	end.	

%% Request Handlers

handle_get()	->
	case lists:reverse(Request#request.path) of
		[MessageId, "messages" | _Destination ] when length(_Destination) > 0 ->
			get_message(MessageId);
		[BatchId, "batches" | Destination] ->
			get_batch(lists:reverse(Destination), BatchId);
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
		["batches", "incomming" | Destination] ->
			with_destination(lists:reverse(Destination), fun post_batch/1);
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
	
%% Private API Implementation
get_message(MessageId) ->
	case yarmo_message:find(Store, MessageId) of
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
		{{rel, "last-batch"}, {path, "poller/batches/last"}}
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

	LinkHeader = yarmo_link_util:link_header(Links),
	Headers = [
	    LinkHeader,
		{'Cache-Control', "public, max-age=\"86400\""},
		{'Expires', expires_header(erlang:universaltime(), 86400)},
		{'ETag', make_etag(LinkHeader)}
	],
	{200, Headers, []}.
			
%% Messages
post_message(#destination{name = Name} = Destination) ->
	MessageUrlFun = location_url(Name),

	Msg = create_message(Destination, Request),
	{201, [{'Location', MessageUrlFun(message, Msg#message.id)}], []}.		

create_message(#destination{id = DestId, max_ttl = MaxTtl}, #request{headers = Headers, body = Body, params = Params}) ->
	Document = #message {
		destination = DestId, 
		max_ttl     = MaxTtl, 
		headers     = Headers,
		body        = Body,
		id          = get_option('message_id', generated, Params)
	}, 
	yarmo_message:create(Store, Document).

%% Message Batches	
post_batch(#destination{name = Name} = Destination) ->
	UrlFun = location_url(Name),

	Batch = yarmo_message:create_batch(Store, #batch{destination = Destination#destination.id, max_ttl = Destination#destination.max_ttl}),
	
	MsgFun = fun(#request{} = MsgReq, Acc) ->
		Msg = create_message(Destination, MsgReq),
		[UrlFun(message, Msg#message.id) | Acc]
	end,	
	Body = lists:foldr(MsgFun, [], parse_batch_body()),
	
	{201, [{'Content-Type', "text/uri-list"}, {'Location', UrlFun(batch, Batch#batch.id)}], string:join(Body, "\r\n") }.			

parse_batch_body() ->
	#request{headers = Headers} = Request,
	case get_option('Content-Type', unknown, Headers) of
		unknown -> [];
		[$m,$u,$l,$t,$i,$p,$a,$r,$t,$/ | _] -> yarmo_web_multipart:parse_multipart_request(Request);
		"application/atom+xml" -> yarmo_web_atom:parse_atom_request(Request);
		Any -> 
			?LOG("Unhandled Content:", [Any]), []
	end.	

%% Destinations	
create_destination(#destination{} = Destination) ->
	Header = fun(Name, Default) -> 
		Value = integer_to_list(Default),
		list_to_integer(get_option(Name, Value, Request#request.headers)) 
	end,
	
	MaxTtl = Header("Message-Max-Ttl", Destination#destination.max_ttl),
	ReplyTime = Header("Message-Reply-Time", Destination#destination.reply_time),
	
	Dest = yarmo_destination:create(Store, Destination#destination{max_ttl = MaxTtl, reply_time = ReplyTime}),

	{201, [ {'Location', destination_url(Destination#destination.name)}, 
			{'Message-MAX-TTL', integer_to_list(Dest#destination.max_ttl)},
			{'Message-Reply-Time', integer_to_list(Dest#destination.reply_time)} ], []}.

%% Filters
with_destination(Name, FoundCallback) ->
	with_destination(Name, FoundCallback, fun(_D) -> {404, [], <<"Not Found.">>} end).

with_destination(Name, FoundCallback, NotFoundCallback) ->
	Destination = name_to_destination(Name),
	case yarmo_destination:find(Store, Destination) of
		not_found -> NotFoundCallback(Destination);
		Dest      -> FoundCallback(Dest)
	end.

%% Utility Functions

name_to_destination(Name) ->
	Type = case ?l2a(Request#request.context_root) of
		queues -> "queue";
		topics -> "topic"
	end,
	#destination{type = Type, name = Name}.
	
get_option(Name, Default, Options) ->
	Fun = fun(Key, Res) ->
		case lists:keysearch(Key, 1, Options) of
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

location_url(Destination) ->
	fun(Type, Id) ->
		Suffix = case Type of
			message -> "/messages/";
			batch   -> "/batches/";
			_       -> "/"
		end,
		destination_url(Destination) ++ Suffix ++ Id	
	end.	
	
destination_url(Destination) ->
	"/" ++ Request#request.context_root ++ "/" ++ string:join(Destination, "/").	

full_destination_url(Scheme, DestName, Path) ->
	Host = get_option('Host', [], Request#request.headers),
	Scheme ++ "://" ++ Host ++ destination_url(DestName) ++ "/" ++ Path.		
	
expires_header(DateTime, TtlSeconds) ->
	Sec = calendar:datetime_to_gregorian_seconds(DateTime),
	ExpireDatetime = calendar:gregorian_seconds_to_datetime(Sec + TtlSeconds),
	httpd_util:rfc1123_date(ExpireDatetime).

make_etag(Term) ->
    <<SigInt:128/integer>> = erlang:md5(term_to_binary(Term)),
    "\"" ++ lists:flatten(io_lib:format("~.36B",[SigInt])) ++ "\"".
	