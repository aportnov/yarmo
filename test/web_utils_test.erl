-module(web_utils_test).
-author('author <alex.portnov@gmail.com>').

-include_lib("eunit/include/eunit.hrl").
-include("../src/yarmo.hrl").

get_header_test() ->
	Mod = handler_mod(),
	Headers = [{'Host', "www.sample.com"}, {'Cache-Control', "private max-age=20"}],
	"www.sample.com" = Mod:get_header('Host', [], Headers),
	"default" = Mod:get_header('Something', "default", Headers).
	
link_header_builder_test() ->
	Mod = handler_mod(),
	Relationships = [
		{{rel, "post-message"}, {path, "incoming"}},
		{{rel, "post-batch"}, {path, "incoming/batches"}}
	],
	
	Builder = Mod:link_header_builder(Relationships, "topics"),
	
	{'Link', 
		"<http://host/topics/sample/incoming>; rel=\"post-message\"," ++ 
		" <http://host/topics/sample/incoming/batches>; rel=\"post-batch\""} = Builder(["sample"], "host").

expires_header_test() ->
	Mod = handler_mod(),
	DateTime = {{2009,12,19},{18,55,10}},
	"Sun, 20 Dec 2009 00:55:48 GMT" = Mod:expires_header(DateTime, 38).
			
make_etag_test() ->
	Mod = handler_mod(),
	"\"57PUN57VQTDZC1BCHRIRIMC7H\"" = Mod:make_etag({'Link', "Sample Link"}). 	
	
handler_mod() ->
	yarmo_web_handler:new(#request{}, mock_store:new({})).			