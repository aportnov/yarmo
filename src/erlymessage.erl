%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(erlymessage).
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
%% @doc Start the erlymessage server.
start() ->
    erlymessage_deps:ensure(),
    ensure_started(crypto),
    application:start(erlymessage).

%% @spec stop() -> ok
%% @doc Stop the erlymessage server.
stop() ->
    Res = application:stop(erlymessage),
    application:stop(crypto),
    Res.
