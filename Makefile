PROJECT_NAME=erlchat

all: reqs compile run

reqs:
	rebar get-deps

compile: clean
	rebar compile

clean:
	rebar clean

run:
	rebar compile
	erl -sname $(PROJECT_NAME) -pa apps/*/ebin deps/*/ebin -eval '[application:start(A) || A <- [kernel, syntax_tools, compiler, goldrush, lager, sync, erlchatsrv, erlchat_client]]'

client:
	rebar compile
	erl -sname $(PROJECT_NAME) -pa apps/*/ebin deps/*/ebin -eval '[application:start(A) || A <- [kernel, syntax_tools, compiler, goldrush, lager, sync, erlchat_client]]'

server:
	rebar compile
	erl -sname $(PROJECT_NAME) -pa apps/*/ebin deps/*/ebin -eval '[application:start(A) || A <- [kernel, syntax_tools, compiler, goldrush, lager, sync, erlchatsrv]]'