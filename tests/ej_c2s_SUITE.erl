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
    ok = application:load(ej),
    %% see ejabberd_config:set_opts failed on line 627
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
     hello_xmpp_server
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
    [{ port_gc_pid, PortGcPid },
     { random_pid, RandomPid }
     | Config].
stop(Config)->
    PortGcPid = proplists:get_value(port_gc_pid, Config),
    RandomPid = proplists:get_value(random_pid, Config),

    true = is_process_alive(RandomPid),
    randoms:stop(),
    false = is_process_alive(RandomPid),

    true = translate:stop(),

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
    Vars0 = ej_c2s:new(),
    true = is_map(Vars0),
    Vars1 = ej_c2s:ul({tcp, 1,  << "<?xml version='1.0'?>" >>}, Vars0),
    true = is_map(Vars1),
    Data = << "<stream:stream to='localhost' xmlns:stream='http://etherx.jabber.org/streams' xmlns='jabber:client' version='1.0'>" >>,
    Vars2 = ej_c2s:ul({tcp, 1,  Data}, Vars1),
    true = is_map(Vars2),
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

replay_event(#event{
                detail_level = DetailLevel,
                trace_ts = TraceTs,
                event_ts = _EventTs,
                from = From,
                to = To,
                label = Label,
                contents = Contents
               }, Config ) ->
    {{Year,Month, Day}, {Hour, Minute, Second} } = calendar:now_to_local_time(TraceTs),
    ct:log
        %%io:format
      ("~p:~p:~p ~p:~p:~p ~p |~p| -----> ~p -----> ~p ~n                    ~p~n"
          ,[Year, Month, Day, Hour, Minute, Second,
            DetailLevel, From,Label, To,
            Contents]),
    Config.

do_it(ActionLists,Config) ->
    lists:foldl
      (fun (F,ConfigTemp) -> ?MODULE:F(ConfigTemp) end,
       Config,
       ActionLists).
