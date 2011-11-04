-module(misultin_rest).
-export([start/1, stop/0]).
-include_lib("misultin/include/misultin.hrl").

%% START MISULTIN SERVER
start(Port) ->
    misultin:start_link([{port, Port}, {loop, fun(Req) -> handle_http(Req) end}]).

%% STOP MISULTIN
stop() ->
    misultin:stop().

%% CALLBACK FUNCTION CALLED ON INCOMING HTTP REQUEST
handle_http(Req) ->
    % dispatch to rest
    handle(Req:get(method), Req:resource([lowercase, urldecode]), Req).

%% MAIN LOGIN PAGE
handle('GET', [], Req) ->
	Req:ok("<!DOCTYPE HTML> 
			<html> 
				<title> Welcome </title>
				<body> Please enter a username to proceed:
					<form name ='input' action = '/createuser/' method ='post'><input type ='text' name ='username'><input type ='submit'></form>
				</body>
			</html>");

%% CREATING NEW USER
handle('POST', ["createuser"], Req) ->
	[{_Us, Username}] = Req:parse_post(),
	Record = Req:raw(),
	IP = Record#req.peer_addr,
	{ok, Pid} = client_sup:start_child(),
	User = {Username, IP, Pid},
	server:add_user(User),
	Req:ok("<!DOCTYPE HTML>
			<html>
				<head>
					<title>Your Page Title</title>
					<meta http-equiv='REFRESH' content='1;url=/channels'>
				</HEAD>
				<BODY>
					Please wait. You are being directed to the Channels page.
				</BODY>
			</HTML>"),
	io:format("~p~p", [IP, Username]);
	
%% CHANNELS LIST PAGE
handle('GET', ["channels"], Req) ->
	Channels = lists:map(fun({Key, _Rest}) -> Key end, server:get_channels()),
	Req:ok("<!DOCTYPE HTML> <html><body><ul>" ++ format_channels(Channels, "") ++ "</ul></body></html>");
	
%% CHANNEL PAGE
handle('GET', ["channels", Channel], Req) ->
	List = server:get_msgs(Channel),
	Req:ok("<!DOCTYPE HTML> 
			<html> 
				<head>
					<meta http-equiv='refresh' content='10'>
				</head>
				<body> 
					<ul>" ++ format_msgs(List, "") ++ "</ul> 
					<form name ='input' action = '/channels/" ++ Channel ++ "/create' method ='post'>
						<input type ='text' name ='message'>
						<input type ='submit'>
					</form>
				</body>
			</html>");

%% SUBMITTING A MESSAGE
handle('POST', ["channels", Channel, "create"], Req) ->
	[{_Mes, Message}] = Req:parse_post(),
	Record = Req:raw(),
	IP = Record#req.peer_addr,
 	[[Username, Pid]] = server:get_user(IP),
	client:send_message({Username, IP, Pid}, Channel, Message),
	Req:ok("<!DOCTYPE HTML>
			<html>
				<head>
					<title>Your Page Title</title>
					<meta http-equiv='REFRESH' content='0;url=/channels/" ++ Channel ++ "/'>
				</HEAD>
			</HTML>");

%% PAGE 404 - NOT FOUND
handle(_, _, Req) ->
    Req:ok("Page not found.").
    
format_msgs([{{Name, IP, PID}, Message} | T], Acc) ->
	Acc1 = Acc ++ "<li>" ++ Name ++ ": " ++ Message ++ "</li>",
	format_msgs(T, Acc1);
format_msgs([], Acc) ->
	Acc.
	
format_channels([H|T], Acc) ->
	Acc1 = Acc ++ "<li><a href='/channels/" ++ H ++ "'>" ++ H ++ "</a></li>",
	format_channels(T, Acc1);
format_channels([], Acc) ->
	Acc.