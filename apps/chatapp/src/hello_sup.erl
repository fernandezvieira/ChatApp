-module(hello_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).
	
init([]) ->
	Client = {misultin_rest, {misultin_rest, start, [8080]}, permanent, 5000, worker, [misultin_rest]},
	{ok, { {one_for_one, 5, 10}, [Client]} }.