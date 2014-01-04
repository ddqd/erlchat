-module(server_logic).
 
-behaviour(gen_server).

-export([stop/0]).
 
-export([start_link/1, init/1]).
 
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
-record(state, {name, socket, group, answer}).

-define(TIMEOUT, 1000).

start_link(Socket) ->
	gen_server:start_link(?MODULE, [Socket], []).

event_loop(State) ->
	lager:log(info, self(), "client pid ~p", [self()]),
	    case gen_tcp:recv(State#state.socket, 0) of
        {ok, Data} ->
            gen_tcp:send(State#state.socket, Data),
            event_loop(State);
        {error, closed} ->
            ok
    end.

receive_handler(State) ->
	Socket = State#state.socket,
	inet:setopts(Socket, [{active, once}]),
	receive
		{tcp, Socket, <<"quit", _/binary>>} ->
			gen_tcp:close(Socket);
		{tcp, Socket, Msg} ->
			gen_tcp:send(Socket, Msg),
			receive_handler(State)
	end.

init([Socket]) ->
	{ok, S} = gen_tcp:accept(Socket),
	gen_server:cast(erlchat_srv, {accepted, self()}),
	Res = gen_tcp:controlling_process(S, self()),
	lager:log(info, self(), "test processs___", [Res]),
	State = #state{socket = S, group=clients},
	{ok, State}.


handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(start, State) ->
	lager:log(info, self(), "start", []),
	{noreply, State};


handle_cast(accept, State) ->
	{noreply, State};

handle_cast(test, State) ->
	lager:log(info, self(), "test pooq", []),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(start, State) ->
	% lager:log(info, self(), "test pooq", []),
 % receive_handler(State),
  {noreply, State};

handle_info(test, State) ->
	lager:log(info, self(), "test pooq", []),
  {noreply, State};

handle_info(Info, State) ->
	lager:log(info, self(), "info", [Info]),
	gen_tcp:controlling_process(State#state.socket, self()),
  {noreply, State}.
  
stop() ->
    gen_server:call({local, ?MODULE}, stop).
 
terminate(_Reason, _State) ->
  ok.
 
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


