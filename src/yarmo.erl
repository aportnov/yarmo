%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(yarmo).
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
%% @doc Start the yarmo server.
start() ->
    yarmo_deps:ensure(),
    ensure_started(crypto),
    ensure_started(ibrowse),
    application:start(yarmo).

%% @spec stop() -> ok
%% @doc Stop the yarmo server.
stop() ->
    Res = application:stop(yarmo),
    application:stop(crypto),
    application:stop(ibrowse),
    Res.
