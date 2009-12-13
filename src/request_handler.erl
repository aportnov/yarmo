-module(request_handler).
-author('author <alex.portnov@gmail.com>').

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{handle, 1}];
behaviour_info(_Other) ->
    undefined.