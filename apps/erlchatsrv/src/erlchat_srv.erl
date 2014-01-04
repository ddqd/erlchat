-module(erlchat_srv).
 
-behaviour(gen_server).
 
 -export([start_link/1, stop/0]).
 
-export([init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
-define(TCP_OPTIONS, [binary, {packet, 0}, {reuseaddr, true}, {nodelay, true}, {active, true}]).

-record(state, {port, smodule, ip=any, socket=null}).

start_link(ServerModule) ->
	State=#state{port = 7000, smodule = ServerModule},
	lager:log(info, self(), "start_link", []),
	gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

init(State) ->
	pg2:create(clients),
	lager:log(info, self(), "init ~p", [State]),
	case gen_tcp:listen(State#state.port, ?TCP_OPTIONS) of
		{ok, Socket} ->
			NewState = State#state{socket = Socket},
			lager:log(info, self(), "state ~p", [NewState]),
			{ok, accept(NewState)};
		{error, Reason} ->
	{stop, Reason}
	end.
 
handle_call(test, _From, State) ->
  Reply = ok,
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(test, State) ->
	lager:log(info, self(), "state ~p", [test_message]),
	{noreply, State};

handle_cast({accepted, Pid}, State) ->
	lager:log(info, self(), "~p", [accepted]),
	{noreply, accept(State)};

handle_cast(_Msg, State) ->
  {noreply, State}.
 
handle_info(_Info, State) ->
  {noreply, State}.

stop() ->
    gen_server:call({local, ?MODULE}, stop).
 
terminate(_Reason, _State) ->
  ok.
 
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

accept(State) ->
    {ok, Pid} = gen_server:start_link(State#state.smodule, [State#state.socket], []),
	gen_server:cast(Pid, start),
	lager:log(info, self(), "next client pid ~p", [Pid]),
    pg2:join(clients, Pid),
    State.

