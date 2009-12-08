-module(erlymsg_msg_handler).
-author('author <alex.portnov@gmail.com>').

-include("erlymsg.hrl").

-export([post_message/3, get_message/3]).

post_message({topic, Topic}, Request, Store) ->
	Destination = #destination{type = "topic", name = Topic},
	MessageUrl = "/topics/" ++ string:join(Topic, "/") ++ "/messages/",
	
	post_message(Destination, MessageUrl, Request, Store);
	
post_message({queue, Queue}, Request, Store) ->
	Destination = #destination{type = "queue", name = Queue},
	MessageUrl = "/queues/" ++ string:join(Queue, "/") ++ "/messages/",
	
	post_message(Destination, MessageUrl, Request, Store).
	

post_message(#destination{} = Destination, MessageUrl, Request, Store) ->
	Dest = erlymsg_destination:ensure_exist(Store, Destination),
	Document = #message {
		destination = Dest#destination.id, 
		max_ttl     = Dest#destination.max_ttl, 
		headers     = Request#request.headers,
		body        = Request#request.body
	}, 
	Msg = erlymsg_message:create(Store, Document),

	{201, [{'Location', MessageUrl ++ Msg#message.id}], []}.


get_message(MessageId, _Request, Store) ->
	case erlymsg_message:find(Store, MessageId) of
		not_found ->
			{404, [], []};
		Message ->
			{200, Message#message.headers, Message#message.body}	
	end.				