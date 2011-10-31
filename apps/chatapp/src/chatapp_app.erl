-module(chatapp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	io:format("app"),
	chatapp_sup:start_link(),
    client_sup:start_link().

stop(_State) ->
    ok.
