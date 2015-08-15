-module(ej_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).



%% internal exports
-export([load_nif/0, load_nif_proc/0]).
-define(EJ_LOAD_NIF_PROC_NAME, ej_app_load_nif).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    load_nif(),
    %% misleading name, no process started, just initialization
    %% database.
    translate:start(),
    %% read parameters.
    ejabberd_config:start(),
    %% again misleading name, start nothing but initialize tables. it
    %% must be started after reading parameters.
    cyrsasl:start(),
    ej_sup:start_link().


stop(_State) ->
    ok.

load_nif() ->
    Where = erlang:whereis(?EJ_LOAD_NIF_PROC_NAME),
    NotLoaded = not(not(Where == undefined)),
    error_logger:info_report({Where,NotLoaded, Where == undefined}),
    if NotLoaded ->
            {ok, _Pid} = proc_lib:start(?MODULE, load_nif_proc, []),
            ok;
       true -> ok
    end.

-spec load_nif_proc() -> no_return().
load_nif_proc() ->
    true = register(?EJ_LOAD_NIF_PROC_NAME,self()),
    stringprep:load_nif(),
    p1_yaml:load_nif(),
    p1_sha:load_nif(),
    OsPath = ej_utils:code_module_dir(xml_stream,["priv", "lib"]),
    ok = erl_ddll:load_driver(OsPath, expat_erl),
    proc_lib:init_ack({ok,self()}),
    receive after infinity -> 1 end.



%% dirty hack for 3rd party library
%%
%% no, it is not possible to load nif from module other than itself.
%%
%% p1_yaml_load_nif() ->
%%     {module, p1_yaml} = code:ensure_loaded(p1_yaml),
%%     LibDir = ej_utils:get_so_path(p1_yaml),
%%     SOPath = filename:join(LibDir, "p1_yaml"),
%%     try erlang:load_nif(SOPath, 0)
%%     catch
%%         error:{reload,Msg} ->
%%             io:format("good ~p~n",Msg),
%%             ok
%%     end.
