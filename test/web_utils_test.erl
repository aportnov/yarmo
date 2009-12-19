-module(web_utils_test).
-author('author <alex.portnov@gmail.com>').

-include_lib("eunit/include/eunit.hrl").

get_header_test() ->
	Headers = [{'Host', "www.sample.com"}, {'Cache-Control', "private max-age=20"}],
	"www.sample.com" = yarmo_web_handler:get_header('Host', [], Headers),
	"default" = yarmo_web_handler:get_header('Something', "default", Headers).
	
link_header_builder_test() ->
	Relationships = [
		{{rel, "post-message"}, {path, "incoming"}},
		{{rel, "post-batch"}, {path, "incoming/batches"}}
	],
	
	Builder = yarmo_web_handler:link_header_builder(Relationships, "topics"),
	
	{'Link', 
		"<http://host/topics/sample/incoming>; rel=\"post-message\"," ++ 
		" <http://host/topics/sample/incoming/batches>; rel=\"post-batch\""} = Builder(["sample"], "host").

expires_header_test() ->
	Mod = yarmo_web_handler,
	DateTime = {{2009,12,19},{18,55,10}},
	"Sun, 20 Dec 2009 00:55:48 GMT" = Mod:expires_header(DateTime, 38).
			
make_etag_test() ->
	Mod = yarmo_web_handler,
	"\"57PUN57VQTDZC1BCHRIRIMC7H\"" = Mod:make_etag({'Link', "Sample Link"}). 			