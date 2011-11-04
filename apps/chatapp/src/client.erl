-module(client).

-export([start/0, add_client/2, send_message/3]).

start() ->
	Pid = spawn(fun() -> loop() end),
	{ok, Pid}.
				
add_client(Name, [{User, IP, Pid} | T]) ->
	server:add_client(Name, {User, IP, Pid}),
	add_client(Name, T);
add_client(Name, []) ->
	ok.

send_message(From, To, Message) ->
	server:send_message(From, To, Message).
	
store(Message) ->
	server:store(Message).

loop() ->
	receive
		Message -> io:format("~p~n",[Message])
	end.
	
