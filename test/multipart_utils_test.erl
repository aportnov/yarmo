-module(multipart_utils_test).
-author('author <alex.portnov@gmail.com>').

-include_lib("eunit/include/eunit.hrl").

-include("../src/yarmo.hrl").

-define(TEST_MOD, yarmo_web_multipart).

multipart_boundary_test() ->
	
	Headers = [{'Content-Type',"multipart/mixed; boundary=\"123xxx123\""}, 'Content-Length',"169"],
	Request = #request{headers = Headers},
	"123xxx123" = ?TEST_MOD:multipart_boundary(Request).

mochi_util_parse_header_test() ->
	{"multipart/mixed",[{"boundary","123xxx123"}]} = 
		mochiweb_util:parse_header("multipart/mixed; boundary=123xxx123").

parse_multipart_test() ->
	Body = <<"--123xxx123\r\nContent-Type: image/jpg\r\n\r\n230492304x0230942309x09213098234\r\n--123xxx123\r\nContent-Type: application/json\r\n\r\n{'arbitrary' : 'message'}\r\n--123xxx123--">>,
	Headers = [{'Content-Type',"multipart/mixed; boundary=\"123xxx123\""}, 'Content-Length',"169"],
	Request = #request{headers = Headers, body = Body},
	
	[
		#request{headers = [{'Content-Type', "image/jpg"}], body = <<"230492304x0230942309x09213098234">>, method = 'POST'},
		#request{headers = [{'Content-Type', "application/json"}], body = <<"{'arbitrary' : 'message'}">>, method = 'POST'}
	] = ?TEST_MOD:parse_multipart_request(Request).
		