-module(data_store).
-author('author <alex.portnov@gmail.com>').

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{read, 1}, {create, 1}, {create, 2}, {get_value, 2}, {view, 2}, {view, 3}, {update, 3}];
behaviour_info(_Other) ->
    undefined.