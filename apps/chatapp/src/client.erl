-module(client).

-export([start/0, add_client/2, send_message/3]).

start() ->
	Pid = spawn(fun() -> loop() end),
	{ok, Pid}.
				
add_client(Name, Pid) ->
	server:add_client(Name, Pid).

send_message(From, To, Message) ->
	server:send_message(From, To, Message).

loop() ->
	receive
		Message -> io:format("~p~n",[Message])
	end.
	
