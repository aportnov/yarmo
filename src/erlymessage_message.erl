-module(erlymessage_message).
-author('author <alex.portnov@gmail.com>').

-include("erlymessage.hrl").

-export([create/2]).

%% For testing
-export([headers2json/1, json2headers/1]).

create(Store, #message{} = Message) ->
	Document = [
		{?l2b("type"), ?l2b("message")},
		{?l2b("destination"), ?l2b(Message#message.destination) },
		{?l2b("max_ttl"), Message#message.max_ttl },
		{?l2b("headers"), headers2json(filter_entity_headers(Message#message.headers)) },
		{?l2b("body"), Message#message.body },
		{?l2b("created_timestamp"), calendar:datetime_to_gregorian_seconds(erlang:universaltime())}
	],
	{{id, Id}, {rev, _}} = Store:create(Document),
	doc2message(Store, [{<<"_id">>, Id} | Document]).	
	
doc2message(Store, Doc) ->
	#message{
		id                = ?b2l(Store:get_value(Doc, "_id")),
		destination       = ?b2l(Store:get_value(Doc, "destination")),
		body              = ?b2l(Store:get_value(Doc, "body")),
		max_ttl           = Store:get_value(Doc, "max_ttl"),
		headers           = json2headers(Store:get_value(Doc, "headers")),
		created_timestamp = Store:get_value(Doc, "created_timestamp")
	}.

filter_entity_headers(Headers) ->
	NoForward = ['Host', 'Content-Length', 'Accept-Encoding', 'Accept', 'User-Agent'],

	Pred = fun({Name, Value}, L) ->
		case lists:member(Name, NoForward) of
			false ->
				[{Name, Value} | L];
			true  -> L
		end	
	end,	
	lists:foldl(Pred, [], Headers).	
	
headers2json(Headers) ->
	Fun = fun({Name, Value}, Acc) ->
		[{struct, [{name, Name}, {value, ?l2b(Value)}]} | Acc]
	end,	
	lists:reverse(lists:foldl(Fun, [], Headers)).

json2headers(Headers) ->
	Fun = fun({struct, [{name, Name}, {value, Value}]}, Acc) ->
		[{Name, ?b2l(Value)} | Acc]
	end,
	lists:reverse(lists:foldl(Fun, [], Headers)).				