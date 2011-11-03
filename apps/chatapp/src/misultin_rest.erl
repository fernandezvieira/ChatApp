-module(misultin_rest).
-export([start/1, stop/0]).

% start misultin http server
start(Port) ->
    misultin:start_link([{port, Port}, {loop, fun(Req) -> handle_http(Req) end}]).

% stop misultin
stop() ->
    misultin:stop().

% callback function called on incoming http request
handle_http(Req) ->
    % dispatch to rest
    handle(Req:get(method), Req:resource([lowercase, urldecode]), Req).

% handle a GET on /
handle('GET', [], Req) ->
    Req:ok("Main home page.");

% handle a GET on /users
handle('GET', ["users"], Req) ->
	Messages = server:get_msgs("Mig"),
	Req:ok("<!DOCTYPE HTML> <html> <body> <ul>" ++ format_msgs(Messages, "") ++ "</ul></body</html>");
	
handle('GET', ["channels"], Req) ->
	Channels = lists:map(fun({Key, _Rest}) -> Key end, server:get_all()),
	Req:ok("<!DOCTYPE HTML> <html><body><ul>" ++ format_channels(Channels, "") ++ "</ul></body></html>");
	
handle('GET', ["channels", Channel], Req) ->
	Messages = server:get_msgs(Channel),
	Req:ok("<!DOCTYPE HTML> <html> <body> <ul>" ++ format_msgs(Messages, "") ++ "</ul> <form name ='input' action = '/channels/" ++ Channel ++ "/create' method ='post'><input type ='text name ='message'><input type ='submit'></body</html>");
	
handle('POST', ["channels", Channel, "create"], Req) ->
	io:format("~p~n", [Req]),
	Req:ok("Create message");
	
	
% handle a GET on /users/{username}
handle('GET', ["users", UserName], Req) ->
    Req:ok("This is ~s's page.", [UserName]);

% handle a GET on /users/{username}/messages
handle('GET', ["users", UserName, "messages"], Req) ->
    Req:ok("This is ~s's messages page.", [UserName]);

% handle the 404 page not found
handle(_, _, Req) ->
    Req:ok("Page not found.").
    
format_msgs([H|T], Acc) ->
	Acc1 = Acc ++ "<li>" ++ H ++ "</li>",
	format_msgs(T, Acc1);
format_msgs([], Acc) ->
	Acc.
	
format_channels([H|T], Acc) ->
	Acc1 = Acc ++ "<li><a href='/channels/" ++ H ++ "'>" ++ H ++ "</a></li>",
	format_channels(T, Acc1);
format_channels([], Acc) ->
	Acc.