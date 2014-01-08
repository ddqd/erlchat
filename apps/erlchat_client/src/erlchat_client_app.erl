-module(erlchat_client_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	[application:start(A) || A <- [kernel, stdlib, syntax_tools, compiler, goldrush, lager, sync]],
	case erlchat_client_sup:start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
	end.

stop(_State) ->
    ok.
