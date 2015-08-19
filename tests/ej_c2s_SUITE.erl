%%%-------------------------------------------------------------------
%%% @author Wang Chunye <wcy123@gmail.com>
%%% @copyright (C) 2015, Wang Chunye
%%% @doc
%%%
%%% @end
%%% Created : 16 Aug 2015 by Wang Chunye <>
%%%-------------------------------------------------------------------
-module(ej_c2s_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("et/include/et.hrl").
%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    setup_env(),
    %% see ejabberd_config:get_modules_with_options
    %%    it read application keys as below
    %%         {ok, Mods} = application:get_key(ej, modules),
    %% so that we have to load the application
    IsLoaded = is_application_loaded(ej),
    if not(IsLoaded) ->
            ok = application:load(ej);
            %% see ejabberd_config:set_opts failed on line 627
       true -> ok
    end,
    ok = mnesia:create_schema([node()]),
    ok = application:start(mnesia),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(TestCase, Config) ->
    Name = list_to_atom("init_" ++ atom_to_list(TestCase)),
    case erlang:function_exported(?MODULE,Name,1) of
        true -> ?MODULE:Name(Config);
        _ -> Config
    end.
%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(TestCase, Config) ->
    Name = list_to_atom("end_" ++ atom_to_list(TestCase)),
    case erlang:function_exported(?MODULE, Name, 1) of
        true -> ?MODULE:Name(Config);
        _ -> Config
    end.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [
     { group0, [{repeat, 3}], [start_and_stop,configure] }
    ].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [
     test_maybe_start,
     test_get_env,
     {group, group0},
     hello_xmpp_server,
     hell_xmpp_server,
     hello_xmpp_server_unknown_host,
     hello_xmpp_server_blacklist,
     hello_xmpp_server_long_lang,
     hello_xmpp_server_undefined_ip,
     hello_xmpp_server_wrong_version,
     hello_xmpp_server_wrong_password,
     hello_xmpp_server_unknown_ns
    ].

test_maybe_start(_Config) ->
    {ok, Pid} = ej_utils:maybe_start_link(hello, randoms,init,[]),
    Pid = whereis(hello),
    {ok, Pid} = ej_utils:maybe_start_link(hello, randoms,init,[]).

test_get_env(_Config) ->
    true = length(ej_utils:getenv("PATH",":", ":")) > 0,
    os:putenv("TESTVAR",""),
    yes = ej_utils:getenv("TESTVAR",yes).

start_and_stop(Config) ->
    do_it([start, stop], Config).

start(Config)->
    ej_app:load_nif(),
    translate:start(),
    {ok, RandomPid} = randoms:start(),
    {ok, PortGcPid } = ej_port_gc:start_link(),
    {ok, HooksPid } = ejabberd_hooks:start_link(),
    [{ port_gc_pid, PortGcPid },
     { random_pid, RandomPid },
     { hooks_pid, HooksPid }
     | Config].
stop(Config)->
    PortGcPid = proplists:get_value(port_gc_pid, Config),
    RandomPid = proplists:get_value(random_pid, Config),
    HooksPid = proplists:get_value(hooks_pid, Config),
    true = is_process_alive(RandomPid),
    randoms:stop(),
    false = is_process_alive(RandomPid),

    true = translate:stop(),

    true = is_process_alive(HooksPid),
    ok = ejabberd_hooks:stop(), timer:sleep(40),
    false = is_process_alive(HooksPid),

    true = is_process_alive(PortGcPid),
    true = ej_port_gc:stop(PortGcPid),
    false = is_process_alive(PortGcPid).



configure(Config) ->
    do_it([start, configure_start, configure_stop, stop],
          Config).

configure_start(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    os:putenv("EJABBERD_CONFIG_PATH",
              filename:join([DataDir,
                             "ejabberd.yml"])),
    ejabberd_config:start(),
    cyrsasl:start(),
    ejabberd_auth:start(),
    Config.
configure_stop(Config) ->
    ejabberd_config:stop(),
    Config.

init_hello_xmpp_server(Config) ->
    record_event(Config).
end_hello_xmpp_server(Config) ->
    replay_event(Config).

hello_xmpp_server(Config) ->
    do_it([start, configure_start, hello_xmpp_server_1, configure_stop, stop],
          Config).

hello_xmpp_server_1(Config) ->
    {atomic,ok} = ejabberd_auth:try_register(<<"test1">>,<<"localhost">>,<<"123">>),

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
    Output0 = dummy_sink:get_output(Vars3),
    {data, [Output]} = Output0,
    {_,_} = binary:match(Output, [<<"success">>]),
    Config.

init_hell_xmpp_server(Config) ->
    record_event(Config).
end_hell_xmpp_server(Config) ->
    replay_event(Config).

hell_xmpp_server(Config) ->
    do_it([start, configure_start, hell_xmpp_server_1, configure_stop, stop],
          Config).

hell_xmpp_server_1(Config) ->
    Vars0 = ej_c2s:new(),
    true = is_map(Vars0),
    Vars00 = ej_c2s:add_bottom_module(dummy_sink,Vars0),
    Vars1 = ej_c2s:ul({tcp, 1,  << "<?xml version='1.0'?>" >>}, dummy_sink, Vars00),
    true = is_map(Vars1),
    %% xmlns:stream is wrong on purpose.
    Data = << "<stream:stream to='localhost' xmlns:stream='http://etherx.jabber.org/streams___' xmlns='jabber:client' version='1.0'>" >>,
    Vars2 = ej_c2s:ul({tcp, 1,  Data}, dummy_sink,Vars1),
    true = is_map(Vars2),
    {data, [_, Output]} = dummy_sink:get_output(Vars2),
    {_,_} = binary:match(Output, [<<"invalid-namespace">>]),
    Config.

init_hello_xmpp_server_unknown_host(Config) ->
    record_event(Config).
end_hello_xmpp_server_unknown_host(Config) ->
    replay_event(Config).

hello_xmpp_server_unknown_host(Config) ->
    do_it([start, configure_start, hello_xmpp_server_unknown_host_1, configure_stop, stop],
          Config).

hello_xmpp_server_unknown_host_1(Config) ->
    Vars0 = ej_c2s:new(),
    true = is_map(Vars0),
    Vars00 = ej_c2s:add_bottom_module(dummy_sink,Vars0),
    Vars1 = ej_c2s:ul({tcp, 1,  << "<?xml version='1.0'?>" >>}, dummy_sink, Vars00),
    true = is_map(Vars1),
    Data = << "<stream:stream to='unknown_host' xmlns:stream='http://etherx.jabber.org/streams' xmlns='jabber:client' version='1.0'>" >>,
    Vars2 = ej_c2s:ul({tcp, 1,  Data}, dummy_sink,Vars1),
    true = is_map(Vars2),
    {data, [_, Output]} = dummy_sink:get_output(Vars2),
    {_,_} = binary:match(Output, [<<"host-unknown">>]),
    Config.

init_hello_xmpp_server_blacklist(Config) ->
    record_event(Config).
end_hello_xmpp_server_blacklist(Config) ->
    replay_event(Config).

hello_xmpp_server_blacklist(Config) ->
    do_it([start, configure_start, hello_xmpp_server_blacklist_1, configure_stop, stop],
          Config).

hello_xmpp_server_blacklist_1(Config) ->
    HooksPid = proplists:get_value(hooks_pid, Config),
    ct:pal("process is alive ~p~n",[{whereis(ejabberd_hooks),
                                     HooksPid
                                     %is_process_alive(HooksPid)
                                    }]),
    ok = ejabberd_hooks:add(check_bl_c2s, fun (_,_,_) ->
                                                  {true,testing, <<"just_for_testing">>}
                                          end,
                            0),
    Vars0 = ej_c2s:new(),
    true = is_map(Vars0),
    Vars00 = ej_c2s:add_bottom_module(dummy_sink,Vars0),
    Vars1 = ej_c2s:ul({tcp, 1,  << "<?xml version='1.0'?>" >>}, dummy_sink, Vars00),
    true = is_map(Vars1),
    Data = << "<stream:stream to='localhost' xmlns:stream='http://etherx.jabber.org/streams' xmlns='jabber:client' version='1.0'>" >>,
    Vars2 = ej_c2s:ul({tcp, 1,  Data}, dummy_sink,Vars1),
    true = is_map(Vars2),
    {data, [_, Output]} = dummy_sink:get_output(Vars2),
    {_,_} = binary:match(Output, [<<"just_for_testing">>]),
    Config.

init_hello_xmpp_server_long_lang(Config) ->
    record_event(Config).
end_hello_xmpp_server_long_lang(Config) ->
    replay_event(Config).

hello_xmpp_server_long_lang(Config) ->
    do_it([start, configure_start, hello_xmpp_server_long_lang_1, configure_stop, stop],
          Config).

hello_xmpp_server_long_lang_1(Config) ->
    HooksPid = proplists:get_value(hooks_pid, Config),
    ct:pal("process is alive ~p~n",[{whereis(ejabberd_hooks),
                                     HooksPid
                                     %is_process_alive(HooksPid)
                                    }]),
    ok = ejabberd_hooks:add(check_bl_c2s, fun (_,_,_) ->
                                                  {true,testing, <<"just_for_testing">>}
                                          end,
                            0),
    Vars0 = ej_c2s:new(),
    true = is_map(Vars0),
    Vars00 = ej_c2s:add_bottom_module(dummy_sink,Vars0),
    Vars1 = ej_c2s:ul({tcp, 1,  << "<?xml version='1.0'?>" >>}, dummy_sink, Vars00),
    true = is_map(Vars1),
    Data = << "<stream:stream to='localhost' xmlns:stream='http://etherx.jabber.org/streams' xmlns='jabber:client' version='1.0' xml:lang='1234567890123456789012345678901234567890'>" >>,
    Vars2 = ej_c2s:ul({tcp, 1,  Data}, dummy_sink,Vars1),
    true = is_map(Vars2),
    {data, [_, Output]} = dummy_sink:get_output(Vars2),
    {_,_} = binary:match(Output, [<<"just_for_testing">>]),
    Config.

init_hello_xmpp_server_undefined_ip(Config) ->
    record_event(Config).
end_hello_xmpp_server_undefined_ip(Config) ->
    replay_event(Config).

hello_xmpp_server_undefined_ip(Config) ->
    do_it([start, configure_start, hello_xmpp_server_undefined_ip_1, configure_stop, stop],
          Config).

hello_xmpp_server_undefined_ip_1(Config) ->
    HooksPid = proplists:get_value(hooks_pid, Config),
    ct:pal("process is alive ~p~n",[{whereis(ejabberd_hooks),
                                     HooksPid
                                     %is_process_alive(HooksPid)
                                    }]),
    ok = ejabberd_hooks:add(check_bl_c2s, fun (_,_,_) ->
                                                  {true,testing, <<"just_for_testing">>}
                                          end,
                            0),
    Vars0 = ej_c2s:new(),
    true = is_map(Vars0),
    Vars00 = ej_c2s:add_bottom_module(dummy_sink,Vars0),
    Vars01 = ej_vars:set(ip,undefined,ej_tcp_stub,Vars00),
    Vars1 = ej_c2s:ul({tcp, 1,  << "<?xml version='1.0'?>" >>}, dummy_sink, Vars01),
    true = is_map(Vars1),
    Data = << "<stream:stream to='localhost' xmlns:stream='http://etherx.jabber.org/streams' xmlns='jabber:client' version='1.0' xml:lang='1234567890123456789012345678901234567890'>" >>,
    Vars2 = ej_c2s:ul({tcp, 1,  Data}, dummy_sink,Vars1),
    true = is_map(Vars2),
    {data, [_, _, Output]} = dummy_sink:get_output(Vars2),
    {_,_} = binary:match(Output, [<<"mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl">>]),
    Config.

init_hello_xmpp_server_wrong_version(Config) ->
    record_event(Config).
end_hello_xmpp_server_wrong_version(Config) ->
    replay_event(Config).

hello_xmpp_server_wrong_version(Config) ->
    do_it([start, configure_start, hello_xmpp_server_wrong_version_1, configure_stop, stop],
          Config).

hello_xmpp_server_wrong_version_1(Config) ->
    HooksPid = proplists:get_value(hooks_pid, Config),
    ct:pal("process is alive ~p~n",[{whereis(ejabberd_hooks),
                                     HooksPid
                                     %is_process_alive(HooksPid)
                                    }]),
    ok = ejabberd_hooks:add(check_bl_c2s, fun (_,_,_) ->
                                                  {true,testing, <<"just_for_testing">>}
                                          end,
                            0),
    Vars0 = ej_c2s:new(),
    true = is_map(Vars0),
    Vars00 = ej_c2s:add_bottom_module(dummy_sink,Vars0),
    Vars01 = ej_vars:set(ip,undefined,ej_tcp_stub,Vars00),
    Vars1 = ej_c2s:ul({tcp, 1,  << "<?xml version='1.0'?>" >>}, dummy_sink, Vars01),
    true = is_map(Vars1),
    Data = << "<stream:stream to='localhost' xmlns:stream='http://etherx.jabber.org/streams' xmlns='jabber:client' version='9.0' xml:lang='en'>" >>,
    Vars2 = ej_c2s:ul({tcp, 1,  Data}, dummy_sink,Vars1),
    true = is_map(Vars2),
    {data, [_, Output]} = dummy_sink:get_output(Vars2),
    {_,_} = binary:match(Output, [<<"wrong version">>]),
    Config.


init_hello_xmpp_server_wrong_password(Config) ->
    record_event(Config).
end_hello_xmpp_server_wrong_password(Config) ->
    replay_event(Config).

hello_xmpp_server_wrong_password(Config) ->
    do_it([start, configure_start, hello_xmpp_server_wrong_password_1, configure_stop, stop],
          Config).

hello_xmpp_server_wrong_password_1(Config) ->
    case  ejabberd_auth:try_register(<<"test1">>,<<"localhost">>,<<"123">>) of
        {atomic,ok} -> ok;
        {atomic,exists} -> ok
    end,
    Vars0 = ej_c2s:new(),
    true = is_map(Vars0),
    Vars00 = ej_c2s:add_bottom_module(dummy_sink,Vars0),
    Vars1 = ej_c2s:ul({tcp, 1,  << "<?xml version='1.0'?>" >>}, dummy_sink, Vars00),
    true = is_map(Vars1),
    Data = << "<stream:stream to='localhost' xmlns:stream='http://etherx.jabber.org/streams' xmlns='jabber:client' version='1.0'>" >>,
    Vars2 = ej_c2s:ul({tcp, 1,  Data}, dummy_sink, Vars1),
    true = is_map(Vars2),
    <<"localhost">> = ej_c2s_state:get_server(Vars2),


    Data2 = <<"<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='PLAIN' xmlns:ga='http://www.google.com/talk/protocol/auth' ga:client-uses-full-bind-result='true'>AHRlc3QxADQ1Ng==</auth>">>,
    Vars3 = ej_c2s:ul({tcp, 1,  Data2}, dummy_sink, Vars2),
    Output0 = dummy_sink:get_output(Vars3),
    {data, [Output]} = Output0,
    {_,_} = binary:match(Output, [<<"not-authorized">>]),
    Config.


init_hello_xmpp_server_unknown_ns(Config) ->
    record_event(Config).
end_hello_xmpp_server_unknown_ns(Config) ->
    replay_event(Config).

hello_xmpp_server_unknown_ns(Config) ->
    do_it([start, configure_start, hello_xmpp_server_unknown_ns_1, configure_stop, stop],
          Config).

hello_xmpp_server_unknown_ns_1(Config) ->
    case  ejabberd_auth:try_register(<<"test1">>,<<"localhost">>,<<"123">>) of
        {atomic,ok} -> ok;
        {atomic,exists} -> ok
    end,
    Vars0 = ej_c2s:new(),
    true = is_map(Vars0),
    Vars00 = ej_c2s:add_bottom_module(dummy_sink,Vars0),
    Vars1 = ej_c2s:ul({tcp, 1,  << "<?xml version='1.0'?>" >>}, dummy_sink, Vars00),
    true = is_map(Vars1),
    Data = << "<stream:stream to='localhost' xmlns:stream='http://etherx.jabber.org/streams' xmlns='jabber:client' version='1.0'>" >>,
    Vars2 = ej_vars:set(output, <<"no_output">>, dummy_sink, Vars1),
    Vars3 = ej_c2s:ul({tcp, 1,  Data}, dummy_sink, Vars2),
    true = is_map(Vars3),
    <<"localhost">> = ej_c2s_state:get_server(Vars3),


    Data2 = <<"<auth xmlns='uri:wrong' mechanism='PLAIN' xmlns:ga='http://www.google.com/talk/protocol/auth' ga:client-uses-full-bind-result='true'>AHRlc3QxADQ1Ng==</auth>">>,
    Vars4 = ej_vars:set(output, <<"no_output">>, dummy_sink, Vars3),
    Vars5 = ej_c2s:ul({tcp, 1,  Data2}, dummy_sink, Vars4),
    <<"no_output">> = dummy_sink:get_output(Vars5),
    Config.


%%%
% internal functions
%%%
setup_env() ->
    Mod = code:which(?MODULE),
    TestDir = filename:dirname(Mod),
    AppDir = filename:dirname(TestDir),
    code:add_patha(filename:join([AppDir, "ebin"])),
    %% now, ej_utils should be available.
    true = code:add_patha(filename:join([AppDir,"deps/esip/ebin"])),
    true = code:add_patha(filename:join([AppDir,"deps/goldrush/ebin"])),
    true = code:add_patha(filename:join([AppDir,"deps/lager/ebin"])),
    true = code:add_patha(filename:join([AppDir,"deps/p1_cache_tab/ebin"])),
    %% true = code:add_patha(filename:join([AppDir,"deps/p1_iconv/ebin"])),
    true = code:add_patha(filename:join([AppDir,"deps/p1_stringprep/ebin"])),
    true = code:add_patha(filename:join([AppDir,"deps/p1_stun/ebin"])),
    true = code:add_patha(filename:join([AppDir,"deps/p1_tls/ebin"])),
    true = code:add_patha(filename:join([AppDir,"deps/p1_utils/ebin"])),
    true = code:add_patha(filename:join([AppDir,"deps/p1_xml/ebin"])),
    true = code:add_patha(filename:join([AppDir,"deps/p1_yaml/ebin"])),
    %% true = code:add_patha(filename:join([AppDir,"deps/p1_zlib/ebin"])),
    true = code:add_patha(filename:join([AppDir,"deps/etcp_server/ebin"])),
    Path = code:get_path(),
    ct:log("seting up env is done ~p~n",[Path])
    .

record_event(Config) ->
    process_flag(trap_exit,true),
    {ok, CollectorPid } =
        et_collector:start_link([{trace_global, true},
                                 {trace_port, 4477},
                                 {trace_pattern, {et,max}}
                                ]),
    [{collector_pid, CollectorPid} | Config].

replay_event(Config) ->
    CollectorPid = proplists:get_value(collector_pid, Config),
    et_collector:iterate(CollectorPid, first, infinity, fun replay_event/2, []),
    catch et_collector:stop(CollectorPid),
    Config.

replay_event(
  %% #event{
  %%               detail_level = DetailLevel,
  %%               trace_ts = TraceTs,
  %%               event_ts = _EventTs,
  %%               from = From,
  %%               to = To,
  %%               label = Label,
  %%               contents = Contents
  %%              } =
  E, Config ) ->
    %% {{Year,Month, Day}, {Hour, Minute, Second} } = calendar:now_to_local_time(TraceTs),
    ct:pal
        %%io:format
      (default, 99,"~s~n", [event_to_string(E, event_ts)]),
    Config.

do_it(ActionLists,Config) ->
    lists:foldl
      (fun (F,ConfigTemp) -> ?MODULE:F(ConfigTemp) end,
       Config,
       ActionLists).

is_application_loaded(App)->
    case lists:keysearch(App,1,application:loaded_applications()) of
        {value, _} -> true;
        false -> false
    end.

%% copy from et_wx_contents_viewer.erl
event_to_string(Event, TsKey) ->
    ReportedTs = Event#event.trace_ts,
    ParsedTs   = Event#event.event_ts,
    Deep =
        ["DETAIL LEVEL: ", ej_utils:term_to_string(Event#event.detail_level),
         "\nLABEL:        ", ej_utils:term_to_string(Event#event.label),
         case Event#event.from =:= Event#event.to of
             true ->
                 ["\nACTOR:        ", ej_utils:term_to_string(Event#event.from)];
             false ->
                 ["\nFROM:         ", ej_utils:term_to_string(Event#event.from),
                  "\nTO:           ", ej_utils:term_to_string(Event#event.to)]
         end,
         case ReportedTs =:= ParsedTs of
             true ->
                 ["\nPARSED:       ", now_to_string(ParsedTs)];
             false ->
                 case TsKey of
                     trace_ts ->
                         ["\nTRACE_TS:     ", now_to_string(ReportedTs),
                          "\nEVENT_TS:     ", now_to_string(ParsedTs)];
                     event_ts ->
                         ["\nEVENT_TS:     ", now_to_string(ParsedTs),
                          "\nTRACE_TS:     ", now_to_string(ReportedTs)]
                 end
         end,
         "\nCONTENTS:\n\n", ej_utils:term_to_string(Event#event.contents)],
    lists:flatten(Deep).

now_to_string({Mega, Sec, Micro} = Now)
  when is_integer(Mega), is_integer(Sec), is_integer(Micro) ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:now_to_universal_time(Now),
    lists:concat([Y, "-",
		  pad_string(Mo, 2, $0, left), "-",
		  pad_string(D, 2, $0, left),
		  "T",
		  pad_string(H, 2, $0, left), ":",
		  pad_string(Mi, 2, $0, left), ":",
		  pad_string(S, 2, $0, left), ".",
		  Micro]);
now_to_string(Other) ->
    ej_utils:term_to_string(Other).
pad_string(Int, MinLen, Char, Dir) when is_integer(Int) ->
    pad_string(integer_to_list(Int), MinLen, Char, Dir);
pad_string(Atom, MinLen, Char, Dir) when is_atom(Atom) ->
    pad_string(atom_to_list(Atom), MinLen, Char, Dir);
pad_string(String, MinLen, Char, Dir) when is_integer(MinLen), MinLen >= 0 ->
    Len = length(String),
    case {Len >= MinLen, Dir} of
        {true, _} ->
            String;
        {false, right} ->
            String ++ lists:duplicate(MinLen - Len, Char);
        {false, left} ->
	    lists:duplicate(MinLen - Len, Char) ++ String
    end.
