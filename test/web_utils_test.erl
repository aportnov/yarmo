-module(web_utils_test).
-author('author <alex.portnov@gmail.com>').

-include_lib("eunit/include/eunit.hrl").
-include("../src/yarmo.hrl").

get_header_test() ->
	Mod = handler_mod(),
	Headers = [{'Host', "www.sample.com"}, {'Cache-Control', "private max-age=20"}],
	"www.sample.com" = Mod:get_option('Host', [], Headers),
	"default" = Mod:get_option('Something', "default", Headers).
	
expires_header_test() ->
	Mod = handler_mod(),
	DateTime = {{2009,12,19},{18,55,10}},
	"Sun, 20 Dec 2009 00:55:48 GMT" = Mod:expires_header(DateTime, 38).
			
make_etag_test() ->
	Mod = handler_mod(),
	"\"57PUN57VQTDZC1BCHRIRIMC7H\"" = Mod:make_etag({'Link', "Sample Link"}). 	
	
handler_mod() ->
	yarmo_web_handler:new(#request{context_root = "topics"}, mock_store:new({})).			