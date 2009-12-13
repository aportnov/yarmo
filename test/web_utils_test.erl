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