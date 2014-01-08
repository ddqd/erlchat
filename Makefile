PROJECT_NAME=erlchat

all: reqs compile run

reqs:
	rebar get-deps

compile: clean
	rebar compile

clean:
	rebar clean

check:
	dialyzer --build_plt --apps kernel stdlib syntax_tools compiler -r deps

test_spec:
	dialyzer --src apps/*/src/

run:
	rebar compile
	erl -sname $(PROJECT_NAME) -pa ebin apps/*/ebin deps/*/ebin -eval 'application:start(erlchat)'

client:
	rebar compile
	erl -sname server -pa apps/*/ebin deps/*/ebin -eval 'application:start(erlchat_client)'

server:
	rebar compile
	erl -sname client -pa apps/*/ebin deps/*/ebin -eval 'application:start(erlchatsrv)'

test: compile
	rebar ct skip_deps=true

test_all: reqs compile test