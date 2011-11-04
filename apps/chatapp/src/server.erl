-module(server).
-behaviour(gen_server).
-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([add_channel/1, add_user/1, send_message/3]).
-compile(export_all).

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	io:format("Started server~n"),
	{ok, {ets:new(?MODULE, [public]), ets:new(userTab, [])}}.
	
%% ===================================================================
%% CALLER FUNCTIONS
%% ===================================================================

%% ADD A NEW CHANNEL TO THE SERVER	
add_channel(Channel) ->
	gen_server:call(?MODULE, {add_channel, string:to_lower(Channel)}).

%% ADD A NEW USER
add_user({Username, IP, Pid}) ->
	gen_server:call(?MODULE, {add_user, {string:to_lower(Username), IP, Pid}}).
	
%% SEND MESSAGE
send_message(From, To, Message) ->
	gen_server:call(?MODULE, {send_message, From, string:to_lower(To), Message}).
	
%% GET CHANNELS
get_channels() ->
	gen_server:call(?MODULE, {get_channels}).
	
%% GET CHANNEL MESSAGES
get_msgs(Channel) ->
	gen_server:call(?MODULE, {get_msgs, string:to_lower(Channel)}).
	
%% GET USER
get_user(IP) ->
	gen_server:call(?MODULE, {get_user, IP}).

%% ===================================================================
%% HANDLE CALL FUNCTIONS
%% ===================================================================

%% ADD A NEW CHANNEL TO THE SERVER
handle_call({add_channel, Channel}, _From, {ChannelTab, UserTab}) ->
	Reply = case ets:lookup(ChannelTab, Channel) of
		[] ->
			ets:insert(ChannelTab, {Channel, []}),
			io:format("Channel: ~p, has been added.~n", [Channel]);
		[{Channel, []}] ->
			io:format("~p already exists.~n")
	end,
	{reply, Reply, {ChannelTab, UserTab}};

%% ADD A NEW USER
handle_call({add_user, {Username, IP, Pid}}, _From, {ChannelTab, UserTab}) ->
	Reply = case ets:lookup(UserTab, {Username, IP, Pid}) of
		[] ->
			ets:insert(UserTab, {Username, IP, Pid}),
			io:format("~p, has been added.~n", [Username]);
		[{Username, _, _}] ->
			io:format("~p, already exists.~n", [Username])
	end,
	{reply, Reply, {ChannelTab, UserTab}};
	
%% SEND MESSAGE
handle_call({send_message, From, To, Message}, _From, {ChannelTab, UserTab}) ->
	Reply = case ets:lookup(ChannelTab, To) of
		[] -> 
			io:format("~p, does not exist.~n", [To]);
		[{To, Messages}] ->
			ets:insert(ChannelTab, {To, [{From, Message} | Messages]}),
			io:format("~p, received: ~p, from ~p.~n", [To, Message, From])
	end,
	{reply, Reply, {ChannelTab, UserTab}};
	
%% GET CHANNELS
handle_call({get_channels}, _From, {ChannelTab, UserTab}) ->
	Reply = case ets:tab2list(ChannelTab) of
		[] ->
			no_channels;
		Object ->
			Object
	end,
	{reply, Reply, {ChannelTab, UserTab}};
	
%% GET CHANNEL MESSAGES
handle_call({get_msgs, Channel}, _From, {ChannelTab, UserTab}) ->
	Reply = case ets:lookup(ChannelTab, Channel) of
		[] -> 
			io:format("No messages in ~p.~n", [Channel]);
		[{Channel, Messages}] ->
			Messages
		end,
	{reply, Reply, {ChannelTab, UserTab}};
	
%% GET USER
handle_call({get_user, IP}, _From, {ChannelTab, UserTab}) ->
	Reply = case ets:match(UserTab, {'$1', IP, '$2'}) of
		[] ->
			io:format("User with IP: ~p doesn't exist.~n", [IP]);
		User ->
			User
		end,
	{reply, Reply, {ChannelTab, UserTab}};
	
handle_call(stop, _From, {ChannelTab, UserTab}) ->
	{stop, normal, stopped, {ChannelTab, UserTab}}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
