-module(client_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1, start_child/0]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).
	
init([]) ->
	Client = {client, {client, start, []}, permanent, 5000, worker, [client]},
	{ok, { {simple_one_for_one, 5, 10}, [Client]} }.

start_child() ->	
	supervisor:start_child(?MODULE, []).