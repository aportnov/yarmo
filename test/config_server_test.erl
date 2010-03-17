-module(config_server_test).
-author('author <alex.portnov@gmail.com>').

-include_lib("eunit/include/eunit.hrl").
-include("../src/yarmo.hrl").

basic_operations_test_() ->
	Options = [{one, 1}, {two, 2}],
	yarmo_config:start(Options),
	[
		fun() -> ?assertEqual(1, yarmo_config:get(one)) end,
		fun() -> ?assertEqual(2, yarmo_config:get(two)) end,
		fun() -> ?assertEqual(not_found, yarmo_config:get(three)) end,
		fun() -> 
			yarmo_config:set(four, 4),
			?assertEqual(4, yarmo_config:get(four)) 
		end,
		fun() -> 
			yarmo_config:set(five, 5),
			?assertEqual(5, yarmo_config:get(five)),
			yarmo_config:delete(five),
			?assertEqual(not_found, yarmo_config:get(five)) 
		end
	].