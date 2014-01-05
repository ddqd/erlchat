
-module(erlchatsrv_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([start_socket/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, temporary, 1000, Type, [I]}).

-define(TCP_OPTIONS, [binary, {packet, 0}, {reuseaddr, true}, {nodelay, true}, {active, true}]).

%% ===================================================================
%% API functions
%% ===================================================================

start_socket() ->
	supervisor:start_child(?MODULE, []).

start_link() ->
	pg2:create(clients),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []),
	start_socket().

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->	
	{ok, Socket} = gen_tcp:listen(7000, ?TCP_OPTIONS),
	{ok, { {simple_one_for_one, 60, 3600}, [?CHILD(server_logic, worker, [Socket])]} }.
