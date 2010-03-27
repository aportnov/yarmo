-module(data_store).
-author('author <alex.portnov@gmail.com>').

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
	 {read, 1}, {create, 1}, {create, 2}, {update, 3}, {delete, 2}, {bulk_delete, 1}, {get_value, 2},
	 {view, 2}, {view, 3}, {create_view, 2}, {create_db, 0}
	];
behaviour_info(_Other) ->
    undefined.