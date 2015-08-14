-module(ej_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).



%% internal exports
-export([ej_load_nif_proc/0]).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ej_load_nif(),
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

ej_load_nif() ->
    Where = erlang:whereis(ej_load_nif),
    NotLoaded = not(not(Where == undefined)),
    error_logger:info_report({Where,NotLoaded, Where == undefined}),
    if NotLoaded ->
            {ok, _Pid} = proc_lib:start(?MODULE, ej_load_nif_proc, []),
            ok;
       true -> ok
    end.

-spec ej_load_nif_proc() -> no_return().
ej_load_nif_proc() ->
    true = register(ej_load_nif,self()),
    stringprep:load_nif(),
    p1_yaml:load_nif(),
    p1_sha:load_nif(),
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
