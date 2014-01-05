-module(server_logic).
 
-behaviour(gen_server).

-export([stop/0]).
 
-export([start_link/1, init/1]).
 
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
-record(state, {socket, group}).

start_link(Socket) ->
	gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
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

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(Info, State) ->
	lager:log(info, self(), "info ~p, PID ~p", [Info, self()]),
	{noreply, State}.
  
stop() ->
	gen_server:call({local, ?MODULE}, stop).
 
terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


