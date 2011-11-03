-module(chatapp_app).
-export([setup/0]).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	io:format("app"),
	chatapp_sup:start_link(),
    client_sup:start_link(),
    hello_sup:start_link().

stop(_State) ->
    ok.

setup() ->
	{ok, C1} = client_sup:start_child(),
	{ok, C2} = client_sup:start_child(),
	client:add_client("Mig", [C1, C2]),
	client:send_message(C1, "Mig", "Hello"),
	client:send_message(C1, "Mig", "Hi"),
	client:send_message(C1, "Mig", "Hello george"),
	client:send_message(C1, "Mig", "Hello tim"),
	client:send_message(C2, "Mig", "How you doing"),
	
	{ok, C3} = client_sup:start_child(),
	{ok, C4} = client_sup:start_child(),
	client:add_client("FB", [C3, C4]),
	client:send_message(C3, "FB", "fb1"),
	client:send_message(C3, "FB", "fb2"),
	client:send_message(C3, "FB", "fb3"),
	client:send_message(C4, "FB", "fb4"),
	client:send_message(C4, "FB", "fb5").
	