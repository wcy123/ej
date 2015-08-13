%% @doc
%% a module for the storage of global variables.
%%
%% A storage is a map, and key is a module name, and value is a map
%% for module specific variables. If key is a value other than a
%% module name, it is really a global variable for the storage.
%%
-module(ej_vars).
-author('wcy123@gmail.com').
-export([
         new/2,
         delete/2,
         get/2,
         get/3,
         set/3,
         set/4,
         return/1
        ]).

%% @doc Vars is a map. Modules is a tuple of modules, a new map is
%% initialized with each modules

new(Modules, Vars) ->
    Size = erlang:tuple_size(Modules),
    for_each_module(Modules, 1, Size, Vars).

get(Module, Vars) ->
    maps:get(Module, Vars).

set(Module, ModuleVars, Vars) ->
    OldModuleVars = maps:get(Module, Vars, #{}),
    NewModuleVars = maps:merge(OldModuleVars,ModuleVars),
    maps:put(Module,NewModuleVars,Vars).

delete(Module, Vars) ->
    maps:remove(Module).

get(Key, Module, Vars) ->
    M = maps:get(Module, Vars),
    maps:get(Key, M).

set(Key, Value, Module, Vars) ->
    M1 = maps:get(Module, Vars),
    M2 = maps:update(Key, Value, M1),
    maps:update(Module, M2, Vars).

%%% internal functions

for_each_module(Modules, Pos, Size, Vars) when Pos =< Size ->
    M = erlang:element(Pos,Modules),
    OldV = maps:get(M,Vars,#{}),
    NewVars0 = Vars#{ M => OldV },
    NewVars1 = M:init(NewVars0),
    for_each_module(Modules, Pos + 1 , Size, NewVars1).
for_each_module(_Modules, _Pos, _Size, Vars) ->
    {ok, Vars}.
