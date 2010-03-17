-module(yarmo_config).
-author('author <alex.portnov@gmail.com>').

-behaviour(gen_server).

-export([start/1, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-export([get/1, set/2, delete/1]).

%% External API

start(Options) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).
	
stop() ->
	gen_server:call(?MODULE, stop).

get(Key) -> gen_server:call(?MODULE, {get, Key}).

set(Key, Value) -> gen_server:call(?MODULE, {set, Key, Value}).

delete(Key) -> 	gen_server:call(?MODULE, {delete, Key}).

%% gen_server callback functions.

init(Options) ->
    Tab = ets:new(?MODULE, []),
	lists:foreach(fun({Name, Value}) -> ets:insert(Tab, {Name, Value}) end, Options),
	{ok, Tab}.

handle_call(stop, _From, State) -> {stop, normal, stopped, State};

handle_call({get, Key}, _From, Tab) -> 
	Reply = case ets:lookup(Tab, Key) of
		[] -> not_found;
		[{Key, Value}] -> Value
	end,
	{reply, Reply, Tab};

handle_call({set, Key, Value}, _From, Tab) -> 
	Reply = (catch ets:insert(Tab, {Key, Value})),
	{reply, Reply, Tab};
	
handle_call({delete, Key}, _From, Tab) -> 
	Reply = (catch ets:delete(Tab, Key)),
	{reply, Reply, Tab}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.	