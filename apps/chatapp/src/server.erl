-module(server).
-behaviour(gen_server).
-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([add_client/2, send_message/3, get_all/0]).
-compile(export_all).

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	io:format("Started server~n"),
	{ok, {ets:new(?MODULE, []), ets:new(msgTab, [])}}.

add_client(Name, Pid) ->
	gen_server:call(?MODULE, {add, string:to_lower(Name), Pid}).

send_message(From, To, Message) ->
	gen_server:call(?MODULE, {message, From, string:to_lower(To), Message}).

get_all() ->
	gen_server:call(?MODULE, {get_all}).
	
get_msgs(Client) ->
	gen_server:call(?MODULE, {get_msgs, string:to_lower(Client)}).
	
get_channels() ->
	gen_server:call(?MODULE, {get_channels}).
	
handle_call({add, Name, Pid}, _From, {ClientTab, MsgTab}) ->
	Reply = case ets:lookup(ClientTab, Name) of
			[] ->	ets:insert(ClientTab, {Name, [Pid]}),
				{Name, has_been_added_and, Pid, has_been_added};
			[{Name, List}] -> 
				case check_pid(Pid, List) of
					true -> {Pid, already_exists_in, Name};
					false -> ets:insert(ClientTab, {Name, [Pid|List]}),
						 {Pid, has_been_added_to, Name}
				end
		end,
	{reply, Reply, {ClientTab, MsgTab}};
	
handle_call({message, From, To, Message}, _From, {ClientTab, MsgTab}) ->
	Reply = case ets:lookup(ClientTab, To) of
			[] -> To, does_not_exist;
			[{To, List}] ->
				case ets:lookup(MsgTab, To) of
					[] -> 
						ets:insert(MsgTab, {To, [Message]});
					[{To, Messages}] ->
						ets:insert(MsgTab, {To, [Message|Messages]})
				end,
			send_msg(From, Message, List, [])
		end,
	{reply, Reply, {ClientTab, MsgTab}};
	
handle_call({get_all}, _From, {ClientTab, MsgTab}) ->
	Reply = case ets:tab2list(ClientTab) of
			[] -> no_clients;
			Object -> Object
		end,
	{reply, Reply, {ClientTab, MsgTab}};
	
handle_call({get_msgs, Client}, _From, {ClientTab, MsgTab}) ->
	Reply = case ets:lookup(MsgTab, Client) of
		[] -> 
			io:format("No messages in ~p~n", [Client]);
		[{Client, Messages}] ->
			Messages
		end,
	{reply, Reply, {ClientTab, MsgTab}};
	
handle_call({get_channels}, _From, {ClientTab, MsgTab}) ->
	Reply = case ets:tab2list(ClientTab) of
		[] ->
			no_channels;
		Object ->
			Object
		end,
	{reply, Reply, {ClientTab, MsgTab}};
	
handle_call(stop, _From, {ClientTab, MsgTab}) ->
	{stop, normal, stopped, {ClientTab, MsgTab}}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

check_pid(Pid, [H|_T]) when Pid =:= H ->
	true;
check_pid(Pid, [_H|T]) ->
	check_pid(Pid, T);
check_pid(_, []) ->
	false.

send_msg(From, Message, [H|T], Recipients) when From =/= H ->
	H ! Message,
	send_msg(From, Message, T, [H|Recipients]);
send_msg(From, Message, [_H|T], Recipients) ->
	send_msg(From, Message, T, Recipients);
send_msg(_From, Message, [], Recipients) ->
	{Recipients, recieved, Message}.
