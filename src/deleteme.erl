-module(deleteme).

-author('author <alex.portnov@gmail.com>').

-include_lib("xmerl/include/xmerl.hrl").
-include("yarmo.hrl").

-export([sample/0]).

-define(TEST_MOD, yarmo_web_atom).

memes_url() ->
  "http://planet.intertwingly.net/memes.atom".

format_entries([]) -> done;
format_entries([Node|Rest]) ->
  [ #xmlText{value=Title} ] = xmerl_xpath:string("title/text()", Node),
  [ #xmlAttribute{value=Link} ] = xmerl_xpath:string("link/@href", Node),
  Message = xmerl:export_simple_content([{a,[{href,Link}],[Title]}],xmerl_xml),
  io:format('~s~n', [xmerl_ucs:to_utf8(Message)]),
  format_entries(Rest).

sample() ->
	application:start(inets),
	  { ok, {_Status, _Headers, Body }} = http:request(memes_url()),
	  { Xml, _Rest } = xmerl_scan:string(Body),
	  format_entries(xmerl_xpath:string("//entry",Xml)),
	  init:stop().	
	
 parse_request_test() ->
	Request = #request{
		body = ?l2b(feed()),
		headers = [{'Content-Type', "application/atom+xml"}]
	},
	[
		#request{},
		#request{},
		#request{}

	] = ?TEST_MOD:parse_atom_request(Request).
	
	
feed() ->
	Feed = "<?xml version='1.0' encoding='utf-8'?>
	<feed xmlns='http://www.w3.org/2005/Atom'>

	  <title>Example Feed</title>
	  <link href='http://example.org/'/>
	  <updated>2003-12-13T18:30:02Z</updated>
	  <author>
	    <name>John Doe</name>
	  </author>
	  <id>urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6</id>

	  <entry>
	    <title>Atom-Powered Robots Run Amok</title>
	    <link href='http://example.org/2003/12/13/atom03'/>
	    <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
	    <updated>2003-12-13T18:30:02Z</updated>
	    <summary>Some text.</summary>
	  </entry>

	</feed>",
	Feed.	