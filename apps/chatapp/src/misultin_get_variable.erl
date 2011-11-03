-module(misultin_get_variable).
-export([start/1, stop/0]).

% start misultin http server
start(Port) ->
    misultin:start_link([{port, Port}, {loop, fun(Req) -> handle_http(Req) end}]).

% stop misultin
stop() ->
    misultin:stop().


% callback on request received
handle_http(Req) ->
    % get params
    Args = Req:parse_qs(),
    case proplists:get_value("value", Args) of
        undefined ->
            Req:ok([{"Content-Type", "text/xml"}], "<misultin_test><error>no value specified</error></misultin_test>");
        Value ->
            Req:ok([{"Content-Type", "text/xml"}], "<misultin_test><value>~s</value></misultin_test>", [Value])
    end.
    
