-module(yarmo_web).
-author('author <alex.portnov@gmail.com>').

-export([start/1, stop/0, loop/2]).

-include("yarmo.hrl").

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

	case RequestData#request.path of
      [Type | Path] when Type =:= "topics"; Type =:= "queues" ->
	
		Handeler = handler(RequestData#request{context_root = Type, path = Path}), 
		Req:respond(Handeler:handle());

       _ ->
		case RequestData#request.method of
			Method when Method =:= 'GET'; Method =:= 'HEAD' ->
				Req:not_found();
			_ -> 
				Req:respond({501, [], <<"Not Implemented.">>})	
		end
	end.	

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

request_data(Req) ->
    "/" ++ Path = Req:get(path),
	Method = Req:get(method),
	{Body, Params} = case Method of
		M when M =:= 'POST'; M =:= 'PUT' -> 
			Binary = Req:recv_body(),
			case Req:get_primary_header_value("content-type") of
                 "application/x-www-form-urlencoded" ++ _ ->
                     {[], mochiweb_util:parse_qs(Binary)};
                 _ ->
                     {Binary, []}
             end;
		_ -> {[], Req:parse_qs()}
	end,
		
	#request{
		method = Method, 
		path = string:tokens(Path, "/;"), 
		peer = Req:get(peer),
		params = Params,
		headers = mochiweb_headers:to_list(Req:get(headers)),
		cookies = Req:parse_cookie(),
		body = case Body of undefined -> <<>>; _ -> Body end
	}.	

handler(#request{} = Request) ->
	Store = yarmo_store,
	yarmo_web_handler:new(Request, Store).		