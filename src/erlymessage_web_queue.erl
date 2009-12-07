-module(erlymessage_web_queue).
-author('author <alex.portnov@gmail.com>').

-export([handle_get/1, handle_post/1]).

-include("erlymessage.hrl").

%% Public API

handle_get(Request) ->	
	
	case lists:reverse(Request#request.path) of
		[MessageId, "messages" | Queue ] ->
			get_message(lists:reverse(Queue), MessageId, Request);
		[BatchId, "batches" | Queue] ->
			get_batch(lists:reverse(Queue), BatchId, Request);
		Queue ->
			get_relationships(lists:reverse(Queue), Request);
		_ ->
			{404, [], []}			
	end.
	
handle_post(Request) ->	
	case lists:reverse(Request#request.path) of
	 	["incoming" | Queue] -> 
			erlymessage_web_msg:post_message({queue, lists:reverse(Queue)}, Request, erlymessage_store);

		_ -> {501, [], []}
	end.	

%% Private Implementation

get_message(_Queue, _Id, _Request) ->
	{501, [], []}.

get_batch(_Queue, _Id, _Request) ->
	{501, [], []}.

get_relationships(Queue, Request) ->
	HostHeader = erlymessage_web_utils:get_header('Host', [], Request#request.headers),

	Relationships = [
		{{rel, "post-message"}, {path, "incoming"}},
		{{rel, "post-batch"}, {path, "incoming/batches"}},
		{{rel, "post-message-once"}, {path, "messages"}},
		{{rel, "post-batch-once"}, {path, "batches"}},
		{{rel, "poller"}, {path, "poller"}}
	],	
	Builder = erlymessage_web_utils:link_header_builder(Relationships, "queues"),	
	Headers = [Builder(Queue, HostHeader)],
	{200, Headers, []}.