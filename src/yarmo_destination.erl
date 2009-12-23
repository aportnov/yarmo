-module(yarmo_destination).
-author('author <alex.portnov@gmail.com>').

-include("yarmo.hrl").

-export([find/2, create/2, generate_key/2]).

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
		{?l2b("created_timestamp"), ?timestamp()}
	],
	Key = generate_key(Type, Name),
	Store:create(Key, Document),
	
	doc2dest(Store, [{<<"_id">>, Key} | Document]).

generate_key(Type, Name) ->
	Type ++ ":" ++ string:join(Name, ".").	

doc2dest(Store, Doc) ->
	BinFun = fun(Value) ->
		case Value of
			Bin when is_binary(Bin) -> ?b2l(Bin);
			Any -> Any
		end	
	end,	
	
	#destination{
		id         = BinFun(Store:get_value(Doc, "_id")),
		type       = BinFun(Store:get_value(Doc, "type")),
		name       = string:tokens(BinFun(Store:get_value(Doc, "name")), "."),
		max_ttl    = Store:get_value(Doc, "max_ttl"),
		reply_time = Store:get_value(Doc, "reply_time")
	}.	
