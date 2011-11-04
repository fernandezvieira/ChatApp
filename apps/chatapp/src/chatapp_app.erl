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
    webpage_sup:start_link().

stop(_State) ->
    ok.

setup() ->
	server:add_channel("mig"),
	server:add_channel("fb"),
	
	{ok, Pid1} = client_sup:start_child(),
	C1 = {"George", 127334, Pid1},
	{ok, Pid2} = client_sup:start_child(),
	C2 = {"Steve", 189883, Pid2},
	{ok, Pid3} = client_sup:start_child(),
	C3 = {"Stacey", 398938, Pid3},
	{ok, Pid4} = client_sup:start_child(),
	C4 = {"Merin", 9479474, Pid4},
	
	server:add_user(C1),
	server:add_user(C2),
	server:add_user(C3),
	server:add_user(C4),
	
	client:send_message(C1, "mig", "Mig1"),
	client:send_message(C1, "Mig", "Hello"),
	client:send_message(C1, "Mig", "Hi"),
	client:send_message(C1, "Mig", "Hello george"),
	client:send_message(C1, "Mig", "Hello tim"),
	client:send_message(C2, "Mig", "How you doing"),
	client:send_message(C3, "FB", "fb1"),
	client:send_message(C3, "FB", "fb2"),
	client:send_message(C3, "FB", "fb3"),
	client:send_message(C4, "FB", "fb4"),
	client:send_message(C4, "FB", "fb5").
	