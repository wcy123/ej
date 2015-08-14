-module(ej_utils).
-export([
         maybe_start_link/4,
         %% start_app_with_dependency/1,
         getenv/1,
         getenv/2,
         getenv/3
         %% app_start_sequence/1, get_so_path/1
        ]).

%% private export
-export([
         maybe_start_link_entry/4
         ]).
%% start_app_with_dependency(App) ->
%%     lists:foreach(
%%       fun(A) -> ok = application:ensure_started(A) end,
%%       app_start_sequence(App)).

%% app_start_sequence(App) ->
%%     app_start_sequence(App, []).

%% app_start_sequence(App,AccApp) ->
%%     application:load(App),
%%     {ok, Deps}  = application:get_key(App, applications),
%%     NewAcc = [App | AccApp -- [App]],
%%     lists:foldl(fun app_start_sequence/2, NewAcc , Deps).

getenv(Env) ->
    getenv(Env, "").
getenv(Env, Default) ->
    case os:getenv(Env) of
        false -> Default;
        "" -> Default;
        Value -> Value
    end.
getenv(Env, Default, Separator) ->
    string:tokens(getenv(Env, Default), Separator).


%% get_so_path(Module) ->
%%     ModuleFile = code:which(Module),
%%     true = filelib:is_file(ModuleFile),
%%     EbinDir = filename:dirname(ModuleFile),
%%     AppDir = filename:dirname(EbinDir),
%%     filename:join([AppDir, "priv", "lib"]).



maybe_start_link(Name,M,F,A)  ->
    case whereis(Name) of
        undefined ->
            {ok, Pid} = proc_lib:start(?MODULE,maybe_start_link_entry,[Name,M,F,A]),
            {ok, Pid};
        Pid -> {ok , Pid}
    end.
maybe_start_link_entry(Name, M,F,A) ->
    true = register(Name, self()),
    proc_lib:init_ack({ok,self()}),
    apply(M,F,A).