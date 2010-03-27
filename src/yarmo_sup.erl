%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Supervisor for the yarmo application.

-module(yarmo_sup).
-author('author <alex.portnov@gmail.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

-define(STORE, yarmo_store).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
	Config = yarmo_deps:local_path(["priv", "conf", "yarmo.ini"]),
	{ok, Options} = file:consult(Config),

	init_database(Options),
	
    Processes = [config_server(), web_process(), background_process()],
    {ok, {{one_for_one, 10, 10}, Processes}}.

%% Private API

web_process() ->
    Ip = case os:getenv("MOCHIWEB_IP") of false -> "0.0.0.0"; Any -> Any end,   
    Config = [
    	{ip, Ip},
        {port, 8000},
        {docroot, yarmo_deps:local_path(["priv", "www"])}
    ],
    {yarmo_web,
           {yarmo_web, start, [Config]},
           permanent, 5000, worker, dynamic}.

config_server() ->
	Modules = [{message, yarmo_message}, {destination, yarmo_destination}],
	
	Options = lists:map(fun({Name, Mod}) -> {Name, Mod:new(?STORE)} end, Modules),
	{yarmo_config,
		{yarmo_config, start, [Options]},
		permanent, 5000, worker, dynamic}.

background_process() ->
	Config = [
		{destinations, fun() -> 
			Mod = yarmo_destination:new(?STORE),
			Mod:find_all() 
		end},
		{expire_messages, fun(Destination) ->
			Mod = yarmo_message:new(?STORE),
			Mod:remove_expired(Destination)
		end},
		{interval, timer:seconds(30)}
	],
	{yarmo_tasks,
		{yarmo_tasks, start, [Config]},
		permanent, 5000, worker, dynamic}.

init_database(Options) ->
	?STORE:create_db(),
	Documents = proplists:get_value(views, Options, []),
	lists:foreach(fun({Name, Views}) -> ?STORE:create_view(Name, Views) end, Documents).