%%%-------------------------------------------------------------------
%%% @author Wang Chunye <wcy123@gmail.com>
%%% @copyright (C) 2015, Wang Chunye
%%% @doc
%%%
%%% @end
%%% Created : 16 Aug 2015 by Wang Chunye <>
%%%-------------------------------------------------------------------
-module(ej_vars_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

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
    ej_app:load_nif(),
    ej_port_gc:start_link(),
    [{logfile, "a.log"} | Config].

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
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

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
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [my_test_case_0].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
my_test_case_0() ->
    [].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
my_test_case_0(_Config) ->
    _Vars = ej_c2s:new(),
    ok.



%%%
% internal functions
%%%
setup_env() ->
    os:putenv("EJABBERD_CONFIG_PATH","ejabberd.yml"),

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

