-module(yarmo_web_util).
-author('author <alex.portnov@gmail.com>').

-export([normalize/1, normalize_key/1, get_option/3, expires/2]).

normalize(L) -> [{normalize_key(Key), Value} || {Key, Value} <- L].

normalize_key(K) when is_list(K) ->
    string:to_lower(K);
normalize_key(K) when is_atom(K) ->
    normalize_key(atom_to_list(K));
normalize_key(K) when is_binary(K) ->
    normalize_key(binary_to_list(K)).

get_option(Name, Default, Options) ->
	Key = normalize_key(Name),
	case lists:keysearch(Key, 1, normalize(Options)) of
		{value, {Key, Value}}  -> Value;
		_ -> Default	
	end.
	
expires(DateTime, TtlSeconds) ->
	Sec = calendar:datetime_to_gregorian_seconds(DateTime),
	ExpireDatetime = calendar:gregorian_seconds_to_datetime(Sec + TtlSeconds),
	httpd_util:rfc1123_date(ExpireDatetime).	