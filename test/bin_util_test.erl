-module(bin_util_test).
-author('author <alex.portnov@gmail.com>').

-include_lib("eunit/include/eunit.hrl").

-include("../src/yarmo.hrl").

-define(TEST_MOD, yarmo_bin_util).

bin_find_exact_match_test() ->
	{exact, 0} = ?TEST_MOD:bin_find(<<"foo">>, <<"foobarbaz">>),
    {exact, 1} = ?TEST_MOD:bin_find(<<"oo">>, <<"foobarbaz">>),
    {exact, 8} = ?TEST_MOD:bin_find(<<"z">>, <<"foobarbaz">>),
    {exact, 0} = ?TEST_MOD:bin_find(<<"foobarbaz">>, <<"foobarbaz">>).

bin_find_partial_match_test() ->
    {partial, 7, 2} = ?TEST_MOD:bin_find(<<"azul">>, <<"foobarbaz">>),
    {partial, 0, 3} = ?TEST_MOD:bin_find(<<"foobar">>, <<"foo">>),
    {partial, 1, 3} = ?TEST_MOD:bin_find(<<"foobar">>, <<"afoo">>).

bin_find_not_found_test() ->
    not_found = ?TEST_MOD:bin_find(<<"q">>, <<"foobarbaz">>),
    not_found = ?TEST_MOD:bin_find(<<"uab">>, <<"foobarbaz">>).		

bin_split_test() ->
	Body = <<"--123xxx123\r\nContent-Type: image/jpg\r\n\r\n230492304x0230942309x09213098234\r\n--123xxx123\r\nContent-Type: application/json\r\n\r\n{'arbitrary' : 'message'}">>,
	Split = ?TEST_MOD:bin_split(<<"--123xxx123\r\n">>, Body),
	[
		<<"Content-Type: image/jpg\r\n\r\n230492304x0230942309x09213098234\r\n">>,
		<<"Content-Type: application/json\r\n\r\n{'arbitrary' : 'message'}">>
	] = Split.	

bin_replace_test() ->
	Body = <<"--123xxx123\\r\\nContent-Type: image/jpg\\r\\n\\r\\n230492304x0230942309x09213098234\\r\\n--123xxx123--">>,
	Expected = <<"--123xxx123\r\nContent-Type: image/jpg\r\n\r\n230492304x0230942309x09213098234\r\n--123xxx123--">>,

	Expected = ?TEST_MOD:bin_replace(?TEST_MOD:bin_replace(Body, <<"\\r">>, <<"\r">>), <<"\\n">>, <<"\n">>).
