%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the erlymsg application.

-module(erlymsg_app).
-author('author <alex.portnov@gmail.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for erlymsg.
start(_Type, _StartArgs) ->
    erlymsg_deps:ensure(),
    erlymsg_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for erlymsg.
stop(_State) ->
    ok.
