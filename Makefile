PROJECT_NAME=erlchat

PLT_NAME=.projectname_dialyzer.plt

run:
	rebar compile
	erl +K true +A30 -sname $(PROJECT_NAME) -pa apps/*/ebin deps/*/ebin -eval '[application:start(A) || A <- [kernel, syntax_tools, compiler, goldrush, lager, sync, erlchatsrv] ]'

 
$(PLT_NAME):
    @ERL_LIBS=../erllib/deps dialyzer --build_plt --output_plt $(PLT_NAME) \
        --apps erts kernel stdlib sasl crypto lager jiffy \
        amqp_client rabbit_common || true
 
dialyze: $(PLT_NAME)
    @dialyzer apps/projectname/ebin --plt $(PLT_NAME) --no_native \
    -Werror_handling -Wunderspecs -Wrace_conditions