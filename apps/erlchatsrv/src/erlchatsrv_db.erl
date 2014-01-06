-module(erlchatsrv_db).

-export([init/0, test/0]).

-export([join/1, leave/1, get_users/0, to_history/2, get_history/1]).

init() ->
	ets:new(users, [set, public, named_table]),
	ets:new(history, [set, public, named_table]).
	
test() ->
	ok.

join(Nick) ->
	ets:insert(users, {Nick, <<"online">>}).

leave(Nick) ->
	ets:insert(users, {Nick, <<"offline">>}).

get_users() ->
	ets:tab2list(users).

to_history(Nick, Message) ->
	case ets:lookup(history, Nick) of 
		[] ->
			ets:insert(history, {Nick, [Message]});
		[{Key,Value}] ->
			ets:insert(history, {Key, Value++[Message]});
		_ -> ok
	end.

get_history(Nick) ->
	ets:lookup(history, Nick).

