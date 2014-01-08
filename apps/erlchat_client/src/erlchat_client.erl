-module(erlchat_client).
 
-behaviour(gen_server).
 
-export([start_link/0, stop/0]).
 
-export([init/1]).
 
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([connect/3, disconnect/0, send/1, send_priv/2, get_history/0, get_history/1, get_users/0]).

-record(state, {nick=[], host, port, socket=[]}).

connect(Nick, Host, Port) ->
	gen_server:cast(?MODULE, {connect,Nick, Host, Port}).

disconnect() ->
	gen_server:call(?MODULE, disconnect).

send(Message) when is_list(Message) ->
	gen_server:call(?MODULE, {send, Message}).

send_priv(To, Message) ->
	gen_server:call(?MODULE, {send_priv, To, Message}).

get_history(Nick) ->
	gen_server:call(?MODULE, {get_history, Nick}).

get_history() ->
	gen_server:call(?MODULE, get_history).

get_users() ->
	gen_server:call(?MODULE, get_users).

-spec init([]) -> {ok, #state{}}.
init([]) ->
	State = #state{},
	{ok, State}.

parse_data(Data, State) ->
	D = jsx:decode(Data),
	% lager:log(info, self(), "valid json ~p \n", [D]),
	case D of
		[{<<"message">>,<<"chat">>},{<<"from">>,Nick},{<<"content">>,Message}] ->
			lager:log(info, self(), "--> [~p]: ~p", [binary_to_list(Nick), binary_to_list(Message)]),
			{noreply, State};
		[{<<"message">>, <<"priv">>},{<<"from">>, Nick},{<<"content">>, Message}] ->
			lager:log(info, self(), "--> [PRIVMSG FROM ~p]: ~p", [binary_to_list(Nick), binary_to_list(Message)]),
			{noreply, State};
		[{<<"cmd">>, <<"history">>}, {<<"user">>, Nick}, {<<"history">>, Message}] ->
			lager:log(info, self(), "--> [HISTORY of ~p]: ~p", [binary_to_list(Nick), binary_to_list(Message)]),
			{noreply, State};
		[{<<"cmd">>, <<"users">>}, {<<"list">>, UsersStatus}] ->
			lager:log(info, self(), "--> [Users status]: ~p", [UsersStatus]),
			{noreply, State};
		[{<<"user">>,<<"join">>},{<<"nick">>,Nick}] ->
			lager:log(info, self(), "--> [User join]: ~p", [binary_to_list(Nick)]),
			{noreply, State};
		[{<<"user">>,<<"leave">>},{<<"nick">>,Nick}] ->
			lager:log(info, self(), "--> [User leave]: ~p", [Nick]),
			{noreply, State};
		_ ->
			{noreply, State}
	end.

join(Socket, Nick) ->
	Body = [{<<"user">>,<<"join">>},{<<"nick">>,list_to_binary(Nick)}],
	gen_tcp:send(Socket, jsx:encode(Body)).

handle_call({send, Message}, _From, State) when State#state.socket =/= [] ->
	Msg = [{<<"message">>, <<"chat">>},{<<"from">>, list_to_binary(State#state.nick)},{<<"content">>, list_to_binary(Message)}],
	Reply = gen_tcp:send(State#state.socket, jsx:encode(Msg)),
	{reply, Reply, State};

handle_call({send_priv, To, Message}, _From, State) when State#state.socket =/= [] ->
	Msg = [{<<"message">>,<<"priv">>},{<<"to">>,list_to_binary(To) },{<<"content">>, list_to_binary(Message)}],
	Reply = gen_tcp:send(State#state.socket, jsx:encode(Msg)),
	{reply, Reply, State};

handle_call({get_history, Nick}, _From, State) when State#state.socket =/= [] ->
	Msg = [{<<"cmd">>,<<"history">>},{<<"user">>,list_to_binary(Nick)}],
	Reply = gen_tcp:send(State#state.socket, jsx:encode(Msg)),
	{reply, Reply, State};

handle_call(get_users, _From, State) when State#state.socket =/= [] ->
	Msg = [{<<"cmd">>, <<"users">>}],
	Reply = gen_tcp:send(State#state.socket, jsx:encode(Msg)),
	{reply, Reply, State};	

handle_call(get_history, _From, State) when State#state.socket =/= [] ->
	Msg = [{<<"cmd">>,<<"history">>},{<<"user">>,list_to_binary(State#state.nick)}],
	Reply = gen_tcp:send(State#state.socket, jsx:encode(Msg)),
	{reply, Reply, State};

handle_call(disconnect, _From, State) when State#state.socket =/= [] ->
	Msg = [{<<"user">>,<<"leave">>},{<<"nick">>, State#state.nick}],
	gen_tcp:send(State#state.socket, jsx:encode(Msg)),
	gen_tcp:send(State#state.socket, jsx:encode(Msg)),
	Reply = gen_tcp:close(State#state.socket),
	{reply, Reply, #state{}};

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast({connect, _, _, _}, State) when State#state.socket =/= [] ->
	lager:log(info, self(), "Already connected", []),
	{noreply, State};

handle_cast({connect, Nick, Host, Port}, State) when State#state.socket == [] ->
  case gen_tcp:connect(Host, Port, [binary, {packet, 0}]) of
  	{ok, Socket} ->
  		gen_tcp:controlling_process(Socket, self()),
  		lager:log(info, self(), "Connected to server ~p: ~p \n", [Host, Port]),
  		JoinResult = join(Socket, Nick),
  		lager:log(info, self(), "Join chat as ~p: ~p \n", [Nick, JoinResult]),
  		NewState = State#state{nick=Nick, host=Host, port=Port, socket=Socket},
  		{noreply, NewState};
  	{error, Reason} ->
  		lager:log(info, self(), "Failed connect to server: ~p \n", [Reason]),
  		{noreply, State}
  end;

handle_cast(_Msg, State) ->
	{noreply, State}.		
 
handle_info({tcp_closed, _Port}, _State) ->
	lager:log(info, self(), "Server disconnected \n", []),
    {noreply, #state{}};

handle_info({tcp_error, _Socket, Reason}, _State) ->
    io:fwrite("Error: ~p~n", [Reason]),
    {stop, normal, #state{}};

handle_info({tcp, _Socket, Data}, State) ->
	case jsx:is_json(Data) of
		true ->
			parse_data(Data, State);
		false ->
			{noreply, State}
	end;

handle_info(_Info, State) ->
	{noreply, State}.

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call({local, ?MODULE}, stop).

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


