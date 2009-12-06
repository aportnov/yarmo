-module(erlymessage_web).
-author('author <alex.portnov@gmail.com>').

-export([start/1, stop/0, loop/2]).

-include("erlymessage.hrl").

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, _DocRoot) ->
	RequestData = request_data(Req),
	
	MethodHandler = fun(Mod, Request) ->
		case Request#request.method of
			Method when Method =:= 'GET'; Method =:= 'HEAD' ->
				Mod:handle_get(Request);
			'POST' ->
				Mod:handle_post(Request);
			_ ->
				{501, [], []}
		end	
	end,	
	?LOG("REQUEST DATA", [RequestData]),
	case RequestData#request.path of
      ["topics" | TopicPath] ->
		Response = MethodHandler(erlymessage_web_topic, RequestData#request{path=TopicPath}),
		Req:respond(Response);
	  ["queues" | QueuePath] ->
		Response = MethodHandler(erlymessage_web_queue, RequestData#request{path=QueuePath}),
		Req:respond(Response);
       _ ->
       	Req:not_found()
	end.	

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

request_data(Req) ->
    "/" ++ Path = Req:get(path),
	Method = Req:get(method),
	Body = case Method of
		M when M =:= 'POST'; M =:= 'PUT' ->
			Req:recv_body();
		_ -> []	
	end,
		
	#request{
		method = Method, 
		path = string:tokens(Path, "/"), 
		peer = Req:get(peer),
		params = Req:parse_qs(),
		headers = mochiweb_headers:to_list(Req:get(headers)),
		cookies = {cookies, Req:parse_cookie()},
		body = Body
	}.