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
	{ok, ets:new(?MODULE, [])}.

add_client(Name, Pid) ->
	gen_server:call(?MODULE, {add, Name, Pid}).

send_message(From, To, Message) ->
	gen_server:call(?MODULE, {message, From, To, Message}).

get_all() ->
	gen_server:call(?MODULE, {get_all}).


handle_call({add, Name, Pid}, _From, Tab) ->
	Reply = case ets:lookup(Tab, Name) of
			[] ->	ets:insert(Tab, {Name, [Pid]}),
				{Name, has_been_added_and, Pid, has_been_added};
			[{Name, List}] -> 
				case check_pid(Pid, List) of
					true -> {Pid, already_exists_in, Name};
					false -> ets:insert(Tab, {Name, [Pid|List]}),
						 {Pid, has_been_added_to, Name}
				end
		end,
	{reply, Reply, Tab};
handle_call({message, From, To, Message}, _From, Tab) ->
	Reply = case ets:lookup(Tab, To) of
			[] -> To, does_not_exist;
			[{To, List}] ->
				send_msg(From, Message, List, [])
		end,
	{reply, Reply, Tab};
handle_call({get_all}, _From, Tab) ->
	Reply = case ets:tab2list(Tab) of
			[] -> no_clients;
			Object -> Object
		end,
	{reply, Reply, Tab};
handle_call(stop, _From, Tab) ->
	{stop, normal, stopped, Tab}.

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
