%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the erlymessage application.

-module(erlymessage_app).
-author('author <alex.portnov@gmail.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for erlymessage.
start(_Type, _StartArgs) ->
    erlymessage_deps:ensure(),
    erlymessage_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for erlymessage.
stop(_State) ->
    ok.
