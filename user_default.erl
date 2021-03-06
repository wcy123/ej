-module(user_default).
-compile([export_all]).
-include("./include/ejabberd_config.hrl").
-include("./include/ejabberd.hrl").
say () ->
    io:format("good~p~n",[self()]).

reload_me() ->
    shell_default:nl(?MODULE).

setup_env() ->
    os:putenv("EJABBERD_CONFIG_PATH","ejabberd.yml"),
    code:add_patha("deps/esip/ebin"),
    code:add_patha("deps/goldrush/ebin"),
    code:add_patha("deps/lager/ebin"),
    code:add_patha("deps/p1_cache_tab/ebin"),
    code:add_patha("deps/p1_iconv/ebin"),
    code:add_patha("deps/p1_stringprep/ebin"),
    code:add_patha("deps/p1_stun/ebin"),
    code:add_patha("deps/p1_tls/ebin"),
    code:add_patha("deps/p1_utils/ebin"),
    code:add_patha("deps/p1_xml/ebin"),
    code:add_patha("deps/p1_yaml/ebin"),
    code:add_patha("deps/p1_zlib/ebin"),
    code:add_patha("deps/etcp_server/ebin"),
    code:add_patha("./tests"),
    code:add_patha("./ebin").

start(ejabberd_config) ->
    mnesia:start(),
    randoms:start(),
    p1_yaml:start(),
    stringprep:start(),
    application:load(ejabberd),
    p1_sha:load_nif(),
    ejabberd_config:srart().

table(T) ->
    mnesia:transaction(fun () -> mnesia:select(T,[{'_',[],['$_']}]) end).
%%ejabberd_config:set_opts().

p(Pid) ->
    case (catch is_process_alive(Pid)) of
        true ->
            io:format("~p~n",
                      [[erlang:process_info(Pid), proc_lib:translate_initial_call(Pid)]]);
        Msg -> io:format("pupu: ~p~n", [Msg])
    end.
tpl(Module) ->
    dbg:tpl(Module,[{'$1',[],[{return_trace}]}]).
tpl(Module,Fun) ->
    dbg:tpl(Module,Fun,[{'$1',[],[{return_trace}]}]).

%% CatchAll = [{'_',[],['$_']}].
%% mnesia:dirty_select(TableName, CatchAll).


%% et_viewer:start([{trace_global, true}, {trace_pattern, {et,max}}]).
maybe_start(Name,M,F,A)  ->
    case whereis(Name) of
        undefined ->
            Pid = case apply(M,F,A) of
                      {ok, XPid} -> XPid;
                      {error, {already_started, XPid}} -> XPid
                  end,
            true = register(Name, Pid),
            Pid;
        Pid -> Pid
    end.

ej_viewer_pid() ->
    maybe_start(ej_viewer,
                et_viewer,
                start,
                [[
                  {trace_global, true},
                  {trace_pattern, {et, max}},
                  {title,"WCY Ejabberd Tracer"},
                  {max_actors,30}
                 ]]).

t0() ->
    _Vars = ej_c2s:new().
t1() ->
    {ok, _Pid } = ej_port_gc:start_link(),
    {ok, Vars0} = ej_c2s:new(),
    {ok, Vars1} =  ej_c2s_vars:ul({tcp, 1,  << "<?xml version='1.0'?>" >>}, Vars0),
    Data = << "<stream:stream to='localhost' xmlns:stream='http://etherx.jabber.org/streams' xmlns='jabber:client' version='1.0'>" >>,
    {ok, Vars2} = ej_c2s_vars:ul({tcp, 1,  Data}, Vars1).


t2() ->
    {ok, _Pid } = ej_port_gc:start_link(),
    {ok, Vars0} = ej_c2s:new(),
    {ok, Vars1} =  ej_c2s_vars:ul({tcp, 1,  << "<?xml version='1.0'?>" >>}, Vars0),
    Data = << "<stream:stream to='localhost' xmlns:stream='http://etherx.jabber.org/streams' xmlns='jabber:client' version='1.0'>" >>,
    {ok, Vars2} = ej_c2s_vars:ul({tcp, 1,  Data}, Vars1).
t3() ->
    ct:run_test([{dir, "tests"}, {logdir,"/var/www/html/log"}, {suite, ej_c2s_SUITE}, {testcase, [start_and_stop]}]).

t4() ->
    ct:run_test([{dir, "tests"}, {logdir,"/var/www/html/log"}, {suite, ej_c2s_SUITE}, {testcase, [configure]}]).

t5() ->
    ct:run_test([{dir, "tests"}, {logdir,"/var/www/html/log"}, {suite, ej_c2s_SUITE}, {testcase, [hello_xmpp_server]}]).

t6() ->
    Vars0 = ej_c2s:new(),
    true = is_map(Vars0),
    Vars00 = ej_c2s:add_bottom_module(dummy_sink,Vars0),
    Vars1 = ej_c2s:ul({tcp, 1,  << "<?xml version='1.0'?>" >>}, dummy_sink, Vars00),
    true = is_map(Vars1),
    Data = << "<stream:stream to='localhost' xmlns:stream='http://etherx.jabber.org/streams' xmlns='jabber:client' version='1.0'>" >>,
    Vars2 = ej_c2s:ul({tcp, 1,  Data}, dummy_sink, Vars1),
    true = is_map(Vars2),
    <<"localhost">> = ej_c2s_state:get_server(Vars2),


    Data2 = <<"<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='PLAIN' xmlns:ga='http://www.google.com/talk/protocol/auth' ga:client-uses-full-bind-result='true'>AHRlc3QxADEyMw==</auth>">>,
    Vars3 = ej_c2s:ul({tcp, 1,  Data2}, dummy_sink, Vars2),
    io:format(user,"~s:~p: [~p] -- ~p~n",[?FILE, ?LINE, ?MODULE, {Vars2, Vars3}]),
    Output0 = dummy_sink:get_output(Vars3),
    ct:pal(default, 99,"~p~n",[{Output0, Vars3}]),
    {data, [Output]} = Output0,
    {_,_} = binary:match(Output, [<<"success">>]).


go() ->
    setup_env(),
    application:ensure_all_started(ej),
    ej_viewer_pid(),
    ej_c2s_SUITE:hello_xmpp_server_1([]).
