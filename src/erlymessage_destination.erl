-module(erlymessage_destination).
-author('author <alex.portnov@gmail.com>').

-include("erlymessage.hrl").

-export([find/2, create/2, ensure_exist/2, generate_key/2]).

find(Store, #destination{type = Type, name = Name}) ->
	case Store:read(generate_key(Type, Name)) of
		not_found -> not_found;
		Doc -> doc2dest(Store, Doc)
	end.	

create(Store, #destination{type = Type, name = Name} = Destination) ->
	Document = [
		{?l2b("type"), ?l2b(Type)},
		{?l2b("name"), ?l2b(string:join(Name, "."))},
		{?l2b("max_ttl"), Destination#destination.max_ttl},
		{?l2b("reply_time"), Destination#destination.reply_time},
		{?l2b("created_timestamp"), calendar:datetime_to_gregorian_seconds(erlang:universaltime())}
	],
	Key = generate_key(Type, Name),
	Store:create(Key, Document),
	
	doc2dest(Store, [{<<"_id">>, ?l2b(Key)} | Document]).

ensure_exist(Store, #destination{type = Type, name = Name} = Destination) ->
	case ?MODULE:find(Store, Destination) of
		not_found -> ?MODULE:create(Store, Destination);
		Dest -> Dest	
	end.
	
generate_key(Type, Name) ->
	Type ++ ":" ++ string:join(Name, ".").	

doc2dest(Store, Doc) ->
	#destination{
		id                = ?b2l(Store:get_value(Doc, "_id")),
		type              = ?b2l(Store:get_value(Doc, "type")),
		name              = ?b2l(Store:get_value(Doc, "name")),
		max_ttl           = Store:get_value(Doc, "max_ttl"),
		reply_time        = Store:get_value(Doc, "reply_time")
	}.	
