-module(multipart_utils_test).
-author('author <alex.portnov@gmail.com>').

-include_lib("eunit/include/eunit.hrl").

-include("../src/yarmo.hrl").

find_in_binary_exact_match_test() ->
	Mod = yarmo_web_multipart,
	{exact, 0} = Mod:find_in_binary(<<"foo">>, <<"foobarbaz">>),
    {exact, 1} = Mod:find_in_binary(<<"oo">>, <<"foobarbaz">>),
    {exact, 8} = Mod:find_in_binary(<<"z">>, <<"foobarbaz">>),
    {exact, 0} = Mod:find_in_binary(<<"foobarbaz">>, <<"foobarbaz">>).

find_in_binary_partial_match_test() ->
	Mod = yarmo_web_multipart,
    {partial, 7, 2} = Mod:find_in_binary(<<"azul">>, <<"foobarbaz">>),
    {partial, 0, 3} = Mod:find_in_binary(<<"foobar">>, <<"foo">>),
    {partial, 1, 3} = Mod:find_in_binary(<<"foobar">>, <<"afoo">>).

find_in_binary_not_found_test() ->
	Mod = yarmo_web_multipart,
    not_found = Mod:find_in_binary(<<"q">>, <<"foobarbaz">>),
    not_found = Mod:find_in_binary(<<"uab">>, <<"foobarbaz">>).		

multipart_boundary_test() ->
	Mod = yarmo_web_multipart,
	
	Headers = [{'Content-Type',"multipart/mixed; boundary=\"123xxx123\""}, 'Content-Length',"169"],
	Request = #request{headers = Headers},
	"123xxx123" = Mod:multipart_boundary(Request).

mochi_util_parse_header_test() ->
	{"multipart/mixed",[{"boundary","123xxx123"}]} = 
		mochiweb_util:parse_header("multipart/mixed; boundary=123xxx123").

parse_multipart_test() ->
	Mod = yarmo_web_multipart,

	Body = <<"--123xxx123\r\nContent-Type: image/jpg\r\n\r\n230492304x0230942309x09213098234\r\n--123xxx123\r\nContent-Type: application/json\r\n\r\n{'arbitrary' : 'message'}\r\n--123xxx123--">>,
	Headers = [{'Content-Type',"multipart/mixed; boundary=\"123xxx123\""}, 'Content-Length',"169"],
	Request = #request{headers = Headers, body = Body},
	
	[
		#request{headers = [{'Content-Type', "image/jpg"}], body = <<"230492304x0230942309x09213098234">>, method = 'POST'},
		#request{headers = [{'Content-Type', "application/json"}], body = <<"{'arbitrary' : 'message'}">>, method = 'POST'}
	] = Mod:parse_multipart_request(Request).

split_binary_test() ->
	Mod = yarmo_web_multipart,

	Body = <<"--123xxx123\r\nContent-Type: image/jpg\r\n\r\n230492304x0230942309x09213098234\r\n--123xxx123\r\nContent-Type: application/json\r\n\r\n{'arbitrary' : 'message'}">>,
	Split = Mod:split_bin(<<"--123xxx123\r\n">>, Body),
	[
		<<"Content-Type: image/jpg\r\n\r\n230492304x0230942309x09213098234\r\n">>,
		<<"Content-Type: application/json\r\n\r\n{'arbitrary' : 'message'}">>
	] = Split.	
	