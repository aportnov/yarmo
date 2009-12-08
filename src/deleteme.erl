-module(deleteme).

-author('author <alex.portnov@gmail.com>').

-include_lib("xmerl/include/xmerl.hrl").

-export([sample/0]).

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
