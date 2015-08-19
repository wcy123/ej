%%%-------------------------------------------------------------------
%%% @author chunywan <wcy123@gmail.com>
%%% @copyright (C) 2015, chunywan
%%% @doc
%%%
%%% @end
%%% Created :  4 Aug 2015 by chunywan <wcy123@gmail.com>
%%%-------------------------------------------------------------------
-module(ej_c2s_state).



%% API

%% callbacks for upper layer
%% ej_c2s_main_loop callbacks
-export([
         new/1,
         ul/2,
         dl/2,
         terminate/2,
         get_stream_id/1,
         change_state/2
        ]).
-spec new(Vars :: ej_vars:ej_vars()) -> ej_vars:ej_vars().
new(Vars) ->
    %% maybe I need to initialize all the states here.
    NewVars1 = ej_vars:new(
                 [
                  %% all states go here
                  ej_c2s_state_wait_for_stream
                 ],
                 Vars#{
                   %% initialization of each of the states
                   ej_c2s_state_wait_for_stream => #{
                     ul_entity => undefined,
                     detail_level => 9,
                     dl_entity => ?MODULE
                    }
                  }),
    ej_vars:add_module(?MODULE,
                       #{
                          %% the initial state
                          ul_entity => ej_c2s_state_wait_for_stream,
                          %% the stream id
                          stream_id => randoms:get_string()
                        },
                       NewVars1).

ul(Args, Vars) ->
    ej_c2s:ul(Args,?MODULE,Vars).

dl({send_xml, _XMLs} = Cmd, Vars) ->
    ej_c2s:dl(Cmd,?MODULE, Vars).

get_stream_id(Vars) ->
    ej_vars:get(stream_id, ?MODULE, Vars).

change_state(State, Vars) ->
    %% OldState = ej_vars:get(ul_entity,?MODULE,Vars),
    %% do I really need to delete the old state to save memory?
    %% NewVars0 = ej_vars:delete(OldState, Vars),
    ej_vars:set(ul_entity, State, ?MODULE, Vars).


terminate(_Args, _Var) ->
    1.
