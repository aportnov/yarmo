%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the yarmo application.

-module(yarmo_app).
-author('author <alex.portnov@gmail.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for yarmo.
start(_Type, _StartArgs) ->
    yarmo_deps:ensure(),
    yarmo_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for yarmo.
stop(_State) ->
    ok.
