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
         add_module/3,
         get/2,
         get/3,
         set/3,
         set/4
        ]).
-type ej_module() :: atom().
-type ej_vars() :: #{ ej_module() => ej_module_vars() }.
-type ej_module_vars() :: #{ atom() => term() }.

%% @doc create module variable storage.
-spec new(Modules :: [ej_module()],
          Vars :: ej_vars()) -> ej_vars().
new(Modules, Vars) ->
    lists:foldl(fun for_each_module/2, Vars,Modules).


-spec add_module(Module :: ej_module(),
                 ModuleVars :: ej_module_vars(),
                 Vars:: ej_vars()) ->
                        ej_vars().
add_module(Module, ModuleVars, Vars) ->
    OldModuleVars1 = maps:get(Module, Vars, #{}),
    OldModuleVars2 = maps:merge(#{name => Module}, OldModuleVars1),
    NewModuleVars = maps:merge(OldModuleVars2,ModuleVars),
    maps:put(Module,NewModuleVars, Vars).

-spec get(Module :: ej_module(),
          Vars:: ej_vars()) ->
                 ej_module_vars().
get(Module, Vars) ->
    maps:get(Module, Vars).


-spec set(Module :: ej_module(),
          ModuleVars :: ej_module_vars(),
          Vars:: ej_vars()) ->
                 ej_vars().
set(Module, ModuleVars, Vars) ->
    OldModuleVars = maps:get(Module, Vars, #{}),
    NewModuleVars = maps:merge(OldModuleVars,ModuleVars),
    maps:put(Module,NewModuleVars,Vars).

-spec get(Key :: atom(),
          Module :: ej_module(),
          Vars :: ej_module_vars())
         -> term().
get(Key, Module, Vars) ->
    M = maps:get(Module, Vars),
    maps:get(Key, M).

-spec set(Key :: atom(),
          Value :: term(),
          Module :: ej_module(),
          Vars :: ej_module_vars())
         -> ej_module_vars().
set(Key, Value, Module, Vars) ->
    M1 = maps:get(Module, Vars),
    M2 = maps:update(Key, Value, M1),
    maps:update(Module, M2, Vars).

%%% internal functions
for_each_module(M, Vars) ->
    OldV = maps:get(M,Vars,#{}),
    NewVars0 = Vars#{ M => OldV },
    M:new(NewVars0).
