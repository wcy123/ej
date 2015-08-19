
DEPSOLVER_PLT=deps/dialyzer.plt

all: compile dialyzer

$(DEPSOLVER_PLT):
	-dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt \
                --apps erts kernel stdlib crypto public_key mnesia sasl -r deps

compile:
	rebar compile skip_deps=true
dialyzer:
	dialyzer --fullpath  --plt $(DEPSOLVER_PLT) -pa deps/p1_xml/ebin -pa deps/lager/ebin -Wrace_conditions --src src

# -no_auto_compile
# -verbosity 99
test: compile
	ct_run -cover cover.spec -cover_stop false   -pa ebin -logdir /var/www/html/log -dir tests

test_x: compile
	ct_run -cover cover.spec -cover_stop false   -pa ebin -logdir /var/www/html/log -dir tests \
	-suite ej_c2s_SUITE -case hello_xmpp_server_unauth_iq
