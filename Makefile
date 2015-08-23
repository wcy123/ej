
DEPSOLVER_PLT=deps/dialyzer.plt

all: compile dialyzer

$(DEPSOLVER_PLT):
	-dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt \
                --apps erts kernel stdlib crypto public_key mnesia sasl et -r deps

compile:
	rebar compile skip_deps=true
dialyzer: $(DEPSOLVER_PLT)
	dialyzer --fullpath  --plt $(DEPSOLVER_PLT) -pa deps/p1_xml/ebin -pa deps/lager/ebin -Wrace_conditions --src src

ERL_SRCS += src/ej_vars.erl
ERL_SRCS += src/ej_c2s.erl
ERL_SRCS += src/ej_tcp_stub.erl
ERL_SRCS += src/ej_xml_stream.erl
ERL_SRCS += src/ej_c2s_state.erl
ERL_SRCS += src/ej_c2s_state_wait_for_stream.erl
ERL_SRCS += src/ej_c2s_state_wait_for_feature_request.erl
ERL_SRCS += src/ej_c2s_state_wait_for_bind.erl
ERL_SRCS += src/ej_c2s_state_wait_for_session.erl
ERL_SRCS += src/ej_c2s_state_session_established.erl

typer:
	dialyzer --fullpath  -pa deps/p1_xml/ebin -pa deps/lager/ebin -Wrace_conditions $(ERL_SRCS) && typer -I include -pa deps/p1_xml/ebin -pa deps/lager/ebin $(ERL_SRCS)
# -no_auto_compile
# -verbosity 99
test: compile
	ct_run -cover cover.spec -cover_stop false   -pa ebin -logdir /var/www/html/log -dir tests

test_x: compile
	ct_run -cover cover.spec -cover_stop false   -pa ebin -logdir /var/www/html/log -dir tests \
	-suite ej_c2s_SUITE -case hello_xmpp_server
