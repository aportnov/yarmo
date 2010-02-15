-module(yarmo_tasks).
-author('author <alex.portnov@gmail.com>').

-behaviour(gen_server).

-export([start/1, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3,
         handle_info/2]).

-export([expire_messages/1, expire_acknowledge/1, loop/1]).

-include("yarmo.hrl").

%% External API

start(Options) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).
	
stop() ->
	gen_server:call(?MODULE, stop).

expire_messages(Destinations) ->
	gen_server:cast(?MODULE, {expire_messages, Destinations}).	

expire_acknowledge(Destinations) ->
	gen_server:cast(?MODULE, {expire_acknowledge, Destinations}).

loop(Options) ->
   Destinations = case get_option(destinations, [], Options) of
		Fun when is_function(Fun) -> Fun();
		L when is_list(L) -> L;
		_ -> []
   end,
   Queues = lists:filter(fun(#destination{type = Type}) -> Type =:= "queue" end, Destinations),
   expire_messages(Destinations),
   expire_acknowledge(Queues).			
	
%% gen_server callback functions.
	
init(Options) ->	
	Interval = get_option(interval, timer:minutes(5), Options),
 	{ok, TRef} = timer:apply_interval(Interval, ?MODULE, loop, [Options]),
	
	{ok, [{timer, TRef} | Options]}.
	
handle_call(stop, _From, State) -> {stop, normal, stopped, State};
handle_call(_Request, _From, State) -> {reply, error, State}.

handle_cast(stop, State) -> {stop, normal, State};		

%% Execute expire function for every destination
handle_cast({Type, Destinations}, State) ->
	Fun = get_option(Type, fun(_D) -> ok end, State),
	lists:foreach(fun(D) -> spawn(fun() -> catch Fun(D) end) end, Destinations),
	{noreply, State};

handle_cast(_Request, State) -> {noreply, State}.

terminate(_Reason, State) ->
	case get_option(timer, undefined, State) of
		undefined -> ok;
		Tref -> timer:cancel(Tref), ok
	end.	

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_info(_Info, State) -> {noreply, State}.

%% Private API	
get_option(Name, Default, Options) ->
	case proplists:lookup(Name, Options) of
		none -> Default;
		{Name, Value} -> Value
	end.
		
		 		