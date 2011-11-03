-module(misultin_hello_world).
-export([start/1, stop/0]).

% start misultin http server
start(Port) ->
    misultin:start_link([{port, Port}, {loop, fun(Req) -> handle_http(Req) end}]).

% stop misultin
stop() ->
    misultin:stop().

% callback on request received
handle_http(Req) ->
    Req:ok("Hello World.").