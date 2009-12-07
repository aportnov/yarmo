-module(erlymessage_web_topic).
-author('author <alex.portnov@gmail.com>').

-export([handle_get/1, handle_post/1]).

-include("erlymessage.hrl").

%% Public API
handle_get(Request)	->
	?LOG("REQUEST", [Request]),
	case lists:reverse(Request#request.path) of
		[MessageId, "messages" | Topic ] ->
			get_message(lists:reverse(Topic), MessageId, Request);
		[BatchId, "batches" | Topic] ->
			get_batch(lists:reverse(Topic), BatchId, Request);
		[] ->
			{404, [], []};
		Topic ->
			get_relationships(lists:reverse(Topic), Request)
	end.	
	
handle_post(Request) ->	
	case lists:reverse(Request#request.path) of
	 	["incoming" | Topic] -> 
			erlymessage_web_msg:post_message({topic, lists:reverse(Topic)}, Request, erlymessage_store);

		_ -> {501, [], []}
	end.	
	
%% Private Implementation

get_message(_Topic, _Id, _Request) ->
	{501, [], []}.

get_batch(_Topic, _Id, _Request) ->
	{501, [], []}.
	
get_relationships(Topic, Request) ->
	HostHeader = erlymessage_web_utils:get_header('Host', [], Request#request.headers),
	
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
	Builder = erlymessage_web_utils:link_header_builder(Relationships, "topics"),	
	Headers = [Builder(Topic, HostHeader)],
	{200, Headers, []}.