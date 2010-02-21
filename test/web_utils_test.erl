-module(web_utils_test).
-author('author <alex.portnov@gmail.com>').

-include_lib("eunit/include/eunit.hrl").
-include("../src/yarmo.hrl").

get_header_test() ->
	Headers = [{'Host', "www.sample.com"}, {'Cache-Control', "private max-age=20"}],
	"www.sample.com" = yarmo_web_util:get_option('Host', [], Headers),
	"default" = yarmo_web_util:get_option('Something', "default", Headers).
	
expires_header_test() ->
	DateTime = {{2009,12,19},{18,55,10}},
	"Sun, 20 Dec 2009 00:55:48 GMT" = yarmo_web_util:expires(DateTime, 38).

normalize_key_test_() ->
	[
		?_assertEqual("host", yarmo_web_util:normalize_key("Host")),
		?_assertEqual("host", yarmo_web_util:normalize_key('Host')),
		?_assertEqual("host", yarmo_web_util:normalize_key(host)),
		?_assertEqual("host", yarmo_web_util:normalize_key(<<"Host">>))
	].

normalize_list_test() ->
	Headers  = [{'Host', "www.sample.com"}, {'Cache-Control', "private max-age=20"}, {"subscriber", "http://some-url"}],
	Expected = [{"host", "www.sample.com"}, {"cache-control", "private max-age=20"}, {"subscriber", "http://some-url"}],
	?assertEqual(Expected, yarmo_web_util:normalize(Headers)).			