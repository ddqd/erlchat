README
======
erlang chat server&client

[![Build Status](https://travis-ci.org/ddqd/erlchat.png?branch=master)](https://travis-ci.org/ddqd/erlchat)

----
###Usage:

1. Make && run: `make all` 
2. Connect to server  `erlchat_client:connect("Username", "localhost", 7000).`
3. Send message: `erlchat_client:send("Message").`
4. Send private message: `erlchat_client:send_priv("ToNick", "message").`
5. Get user list: `erlchat_client:get_users().`
6. Get history `erlchat_client:get_users().`
7. Get history of specified user: `erlchat_client:get_users("Username").`




