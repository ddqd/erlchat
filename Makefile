PROJECT_NAME=erlchat

PLT_NAME=.projectname_dialyzer.plt

run:
	rebar compile
	erl -sname $(PROJECT_NAME) -pa apps/*/ebin deps/*/ebin -eval '[application:start(A) || A <- [kernel, syntax_tools, compiler, goldrush, lager, sync, erlchatsrv, erlchat_client] ]'

 
