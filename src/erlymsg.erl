%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(erlymsg).
-author('author <alex.portnov@gmail.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the erlymsg server.
start() ->
    erlymsg_deps:ensure(),
    ensure_started(crypto),
    application:start(erlymsg).

%% @spec stop() -> ok
%% @doc Stop the erlymsg server.
stop() ->
    Res = application:stop(erlymsg),
    application:stop(crypto),
    Res.
