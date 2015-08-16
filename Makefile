
DEPSOLVER_PLT=deps/dialyzer.plt

all: compile dialyzer

$(DEPSOLVER_PLT):
	-dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt \
                --apps erts kernel stdlib crypto public_key -r deps

compile:
	rebar compile skip_deps=true
dialyzer:
	dialyzer --fullpath  --plt $(DEPSOLVER_PLT) -pa deps/p1_xml/ebin -pa deps/lager/ebin -Wrace_conditions --src src


test:
	ct_run -cover cover.spec -cover_stop false -no_auto_compile -verbosity 99 -pa ebin -logdir /usr/share/nginx/www/log -dir tests