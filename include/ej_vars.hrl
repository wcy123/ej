
%% -define(EJ_MODULE_VARS(Vars),
%% 	ej_vars:get(?MODULE,Vars)).
%% -define(EJ_SET_MODULE_VARS(ModuleVars, Vars),
%% 	ej_vars:set(?MODULE,Vars)).
%% -define(EJ_GET_VAR(VarName, Vars),
%% 	ej_vars:get(VarName, ?MODULE,Vars)).
%% -define(EJ_SET_VAR(VarName, VarValue, Vars),
%% 	ej_vars:set(VarName, VarValue, ?MODULE, Vars)).

-define(INFO_MSG(Format, Args),
	lager:info(Format, Args)).

-define(WARNING_MSG(Format, Args),
	lager:warning(Format, Args)).

-define(ERROR_MSG(Format, Args),
	lager:error(Format, Args)).

-define(CRITICAL_MSG(Format, Args),
	lager:critical(Format, Args)).
