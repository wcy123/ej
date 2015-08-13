%%%-------------------------------------------------------------------
%%% @author chunywan <wcy123@gmail.com>
%%% @copyright (C) 2015, chunywan
%%% @doc
%%%
%%% @end
%%% Created :  4 Aug 2015 by chunywan <wcy123@gmail.com>
%%%-------------------------------------------------------------------
-module(ej_c2s_state).
-include("ej_vars.hrl").


%% API

%% callbacks for upper layer
%% ej_c2s_main_loop callbacks
-export([
         init/0,
         ul/2,
         dl/2,
         terminate/2
        ]).
init(Vars) ->

ul(Args, Vars) ->
    io:format("gogo ~p~n",[Args]),
    {ok, Vars}.

dl({next_state, State}, Vars) ->
    OldState = ej_vars:get(ul_entity,?MODULE,Vars),
    NewVars0 = ej_vars:delete(OldState, Vars),
    NewVars1 = ej_vars:ej_vars:new({ej_c2s_state_wait_for_stream}, NewVars)
    NewModuleVars = OldModuleVars#{
                      ul_entity := ej_c2s_state_wait_for_stream
                     },
    NewVars = ej_vars:set(?MODULE, NewModuleVars, Vars),
    ej_vars:return().
    1.
terminate(_Args, _Var) ->
    1.
