-module(client_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(USER, "TestUser").

all() ->
  [
    connection,
    send_message,
    send_private,
    get_users,
    get_history

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

connection(_Config) ->
  Res = erlchat_client:connect(?USER, "localhost", 7000),
  ?assertEqual(ok, Res).

send_message(_Config) ->
  Res = erlchat_client:send("TestMessage"),
  ?assertEqual(ok, Res).

send_private(_Config) ->
  Res = erlchat_client:send_priv(?USER, "TestMessage"),
  ?assertEqual(ok, Res).

get_users(_Config) ->
  Res = erlchat_client:get_users(),
  ?assertEqual(ok, Res).

get_history(_Config) ->
  Res = erlchat_client:get_history(),
  ?assertEqual(ok, Res).