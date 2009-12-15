-module(multipart_utils_test).
-author('author <alex.portnov@gmail.com>').

-include_lib("eunit/include/eunit.hrl").

-include("../src/yarmo.hrl").

bin_find_exact_match_test() ->
	Mod = yarmo_bin_util,
	{exact, 0} = Mod:bin_find(<<"foo">>, <<"foobarbaz">>),
    {exact, 1} = Mod:bin_find(<<"oo">>, <<"foobarbaz">>),
    {exact, 8} = Mod:bin_find(<<"z">>, <<"foobarbaz">>),
    {exact, 0} = Mod:bin_find(<<"foobarbaz">>, <<"foobarbaz">>).

bin_find_partial_match_test() ->
	Mod = yarmo_bin_util,
    {partial, 7, 2} = Mod:bin_find(<<"azul">>, <<"foobarbaz">>),
    {partial, 0, 3} = Mod:bin_find(<<"foobar">>, <<"foo">>),
    {partial, 1, 3} = Mod:bin_find(<<"foobar">>, <<"afoo">>).

bin_find_not_found_test() ->
	Mod = yarmo_bin_util,
    not_found = Mod:bin_find(<<"q">>, <<"foobarbaz">>),
    not_found = Mod:bin_find(<<"uab">>, <<"foobarbaz">>).		

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

bin_split_test() ->
	Mod = yarmo_bin_util,

	Body = <<"--123xxx123\r\nContent-Type: image/jpg\r\n\r\n230492304x0230942309x09213098234\r\n--123xxx123\r\nContent-Type: application/json\r\n\r\n{'arbitrary' : 'message'}">>,
	Split = Mod:bin_split(<<"--123xxx123\r\n">>, Body),
	[
		<<"Content-Type: image/jpg\r\n\r\n230492304x0230942309x09213098234\r\n">>,
		<<"Content-Type: application/json\r\n\r\n{'arbitrary' : 'message'}">>
	] = Split.	

bin_replace_test() ->
	Mod = yarmo_bin_util,

	Body = <<"--123xxx123\\r\\nContent-Type: image/jpg\\r\\n\\r\\n230492304x0230942309x09213098234\\r\\n--123xxx123--">>,
	Expected = <<"--123xxx123\r\nContent-Type: image/jpg\r\n\r\n230492304x0230942309x09213098234\r\n--123xxx123--">>,

	Expected = Mod:bin_replace(Mod:bin_replace(Body, <<"\\r">>, <<"\r">>), <<"\\n">>, <<"\n">>).
		