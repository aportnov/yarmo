-module(atom_parser_test).
-author('author <alex.portnov@gmail.com>').

-include_lib("eunit/include/eunit.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-include("../src/yarmo.hrl").

-define(TEST_MOD, yarmo_web_atom).

extract_feed_id_test() ->
	{ Xml, _Rest } = xmerl_scan:string(mock_feed()),
	[ #xmlText{value = Id} ] = xmerl_xpath:string("id/text()", Xml),
	"urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6" =  Id.

link_header_test_() ->
	[E1, E2, E3] = entries(mock_feed()),
	[
		?_assertEqual([
			#link{href = "http://example.org/2003/12/13/atom03", rel = ["alternate"]}
		], ?TEST_MOD:parse_link(E1)),
		
		?_assertEqual([
			#link{href = "http://example.org/2003/12/13/atom04", rel = ["related"]}
		], ?TEST_MOD:parse_link(E2)),
		
		?_assertEqual([
			#link{href = "http://example.org/2003/12/13/atom05", rel = ["alternate"], title = "sample", extensions = [{"type", "application/xml"}]},
			#link{href = "http://example.org/2003/12/13/atom07", rel = ["related"]}
		], ?TEST_MOD:parse_link(E3))
	].

entry_id_test_() ->
	[E1, E2, E3] = entries(mock_feed()),
	[
		?_assertEqual("urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a", ?TEST_MOD:parse_id(E1)),
		?_assertEqual("urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa7b", ?TEST_MOD:parse_id(E2)),
		?_assertEqual("urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa8c", ?TEST_MOD:parse_id(E3))
	].

entry_content_test_() ->
	[E1, E2, E3] = entries(mock_feed()),
	[
		?_assertEqual(#content{summary = "Some text.", type = "text/plain"}, ?TEST_MOD:parse_content(E1)),
		?_assertEqual(#content{summary = "Some text.", type = "text", body = "Sample Text Content"}, ?TEST_MOD:parse_content(E2)),
		?_assertEqual(#content{summary = "Some text.", type = "xml", src = "http://www.somelocation.com/cool-feed"}, ?TEST_MOD:parse_content(E3))
	].	

entries(Feed) ->
	{ Xml, _Rest } = xmerl_scan:string(Feed),
	xmerl_xpath:string("//entry", Xml).
	

mock_feed() ->
"<?xml version='1.0' encoding='utf-8'?>
<feed xmlns='http://www.w3.org/2005/Atom'>

  <title>Example Feed</title>
  <link href='http://example.org/'/>
  <updated>2003-12-13T18:30:02Z</updated>
  <author>
    <name>John Doe</name>
  </author>
  <id>urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6</id>

  <entry>
    <title>Atom-Powered Robots Run Amok 1</title>
    <link href='http://example.org/2003/12/13/atom03'/>
    <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
    <updated>2003-12-13T18:30:02Z</updated>
    <summary>Some text.</summary>
  </entry>

  <entry>
    <title>Atom-Powered Robots Run Amok 2</title>
    <link href='http://example.org/2003/12/13/atom04' rel='related' />
    <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa7b</id>
    <updated>2003-12-13T18:30:02Z</updated>
    <summary>Some text.</summary>
	<content type='text'>Sample Text Content</content>
  </entry>

  <entry>
    <title>Atom-Powered Robots Run Amok 3</title>
    <link href='http://example.org/2003/12/13/atom05' type='application/xml' title ='sample'/>
    <link href='http://example.org/2003/12/13/atom07' rel='related' />
    <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa8c</id>
    <updated>2003-12-13T18:30:02Z</updated>
    <summary>Some text.</summary>
	<content type='xml' src='http://www.somelocation.com/cool-feed' />
  </entry>

</feed>".	