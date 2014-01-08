-module(client_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
  [
    ex_test
  ].

init_per_suite(Config) ->
  Client = application:start(erlchat_client),
  Server = application:start(erlchatsrv),
  Config.

init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, Config) ->
  Config.

end_per_suite(Config) ->
  Config.

ex_test(_Config) ->
  ?assertEqual(ok, ok).
