-module(erlchatsrv_worker).
 
-behaviour(gen_server).

-export([stop/0]).
 
-export([start_link/1, init/1]).
 
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
-record(state, {socket, group, nick=[]}).

start_link(Socket) ->

	gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
	pg2:join(clients, self()),
	State = #state{socket = Socket, group=clients},
	gen_server:cast(self(), accept),
	{ok, State}.

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast(accept, State) ->
	{ok, Socket} = gen_tcp:accept(State#state.socket),
	gen_tcp:controlling_process(Socket, self()),
	erlchatsrv_sup:start_socket(),
	{noreply, State};

handle_cast({privMsg, _From, ToNick, MsgBody}, State) when ToNick == State#state.nick->
	io:fwrite("priv_msg received: ~p~n", [self()]),
	gen_tcp:send(State#state.socket, MsgBody),
	{noreply, State};

handle_cast({chatMsg, MsgBody}, State) ->
	io:fwrite("chat_msg received: ~p~n", [self()]),
	gen_tcp:send(State#state.socket, MsgBody),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

parse_data(Data, State) ->
	D = jsx:decode(Data),
	case D of 
		[{<<"user">>,<<"join">>},{<<"nick">>,Nick}] when State#state.nick == []->
			io:fwrite("User joined: ~p~n", [self()]),
			erlchatsrv_db:join(Nick),
			{noreply, State#state{nick=Nick}};
		[{<<"user">>,<<"leave">>}] when State#state.nick =/= [] ->
			erlchatsrv_sup:stop_socket(self()), 
			{noreply, State};
		[{<<"message">>, <<"priv">>},{<<"to">>, ToNick},{<<"content">>, PrivMsg}] ->
			[gen_server:cast(Clients, {privMsg, State#state.nick, ToNick, PrivMsg}) || Clients <- pg2:get_members(clients)],
			{noreply, State};
		[{<<"message">>, <<"chat">>},{<<"content">>, MsgBody}] when State#state.nick =/= [] ->
			erlchatsrv_db:save_message(State#state.nick, MsgBody),
			[gen_server:cast(Clients, {chatMsg,  MsgBody}) || Clients <- pg2:get_members(clients)],
			{noreply, State};
		[{<<"cmd">>,<<"history">>},{<<"user">>, NickName}] when State#state.nick =/= [] ->
			erlchatsrv_db:get_history(NickName),
			{noreply, State};
		_ -> 
			lager:log(info, self(), "unknown message ~p", [D]),
			{noreply, State}
	end.
	
handle_info({tcp_closed, _Port}, State) ->
io:fwrite("User leave: ~p~n", [self()]),
	erlchatsrv_db:leave(State#state.nick),
	erlchatsrv_sup:stop_socket(self()),
    {noreply, State#state{nick=null}};

handle_info({tcp_error, _Socket, Reason}, State) ->
    io:fwrite("Error: ~p~n", [Reason]),
    {stop, normal, State};

handle_info({tcp, Socket, Data}, State) ->
	case jsx:is_json(Data) of
		true ->
			parse_data(Data, State#state{socket = Socket});
		false ->
			{noreply, State}
	end;

handle_info(Mes, State) ->
	io:fwrite("mess: ~p~n", [Mes]),
	{noreply, State}.
  
stop() ->
	gen_server:call({local, ?MODULE}, stop).
 
terminate(_Reason, State) ->
	{stop, normal, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


