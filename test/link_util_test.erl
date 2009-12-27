-module(link_util_test).
-author('author <alex.portnov@gmail.com>').

-include_lib("eunit/include/eunit.hrl").

-include("../src/yarmo.hrl").

-define(TEST_MOD, yarmo_link_util).

parse_link_test_() ->
	Link = #link{href = "http://some.com"},
	[
		?_assertEqual("<http://some.com>; rel=\"alternate\"", ?TEST_MOD:parse_link(Link)),
		
		?_assertEqual("<http://some.com>; rel=\"alternate stylesheet\"", 
			?TEST_MOD:parse_link(Link#link{rel = ["alternate","stylesheet"]})),
			
		?_assertEqual("<http://some.com>; rel=\"alternate\"; title=\"some\"",
			?TEST_MOD:parse_link(Link#link{title = "some"})),
			
		?_assertEqual("<http://some.com>; rel=\"alternate\"; title=\"some\"; type=\"application/xml\"",
			?TEST_MOD:parse_link(Link#link{title = "some", extensions = [{"type", "application/xml"}]}))
			
	].
	
link_header_test() ->
	Links = [
		#link{href = "http://some.com"},
		#link{href = "http://other.com", rel = ["related"], title = "sample"}
	],	
	
	Expected = {'Link', "<http://some.com>; rel=\"alternate\", <http://other.com>; rel=\"related\"; title=\"sample\""},
	
	?assertEqual(Expected, ?TEST_MOD:link_header(Links)).
	 
	