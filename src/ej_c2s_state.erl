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
         set_stream_id/2,
         get_user/1,
         set_user/2,
         get_lang/1,
         set_lang/2,
         change_state/2,
         get_server/1,
         set_server/2,
         get_resource/1,
         set_resource/2,
         get_jid/1,
         set_jid/2,
         get_sid/1,
         set_sid/2,
         get_access/1,
         set_access/2,
         stream_mgmt_enabled/1
        ]).
-spec new(Vars :: ej_vars:ej_vars()) -> ej_vars:ej_vars().
new(Vars) ->
    %% maybe I need to initialize all the states here.
    NewVars1 = ej_vars:new(
                 [
                  %% all states go here
                  ej_c2s_state_wait_for_stream,
                  ej_c2s_state_wait_for_feature_request,
                  ej_c2s_state_wait_for_bind
                 ],
                 Vars#{
                   %% initialization of each of the states
                   ej_c2s_state_wait_for_stream => #{
                     ul_entity => undefined,
                     detail_level => 9,
                     dl_entity => ?MODULE
                    },
                   ej_c2s_state_wait_for_feature_request => #{
                     ul_entity => undefined,
                     detail_level => 9,
                     dl_entity => ?MODULE
                    },
                   ej_c2s_state_wait_for_bind => #{
                     ul_entity => undefined,
                     detail_level => 9,
                     dl_entity => ?MODULE
                    }
                  }),
    ej_vars:add_module(?MODULE,
                       #{
                          %% the initial state
                          ul_entity => ej_c2s_state_wait_for_stream,
                          %% server name
                          server => <<"">>,
                          user => <<"">>,
                          resource => <<"">>,
                          lang => <<"">>,
                          jid => undefined,
                          %% not so clear what the usage of access,
                          %% refer to module acl.
                          access => c2s,
                          sid => undefined,
                          %% the stream id
                          stream_id => randoms:get_string()
                        },
                       NewVars1).

ul(Args, Vars) ->
    ej_c2s:ul(Args,?MODULE,Vars).

dl({send_xml, _XMLs} = Cmd, Vars) ->
    ej_c2s:dl(Cmd,?MODULE, Vars).

get_stream_id(Vars) ->
    ej_vars:get(stream_id,?MODULE, Vars).
set_stream_id(Value,Vars) ->
    ej_vars:set(stream_id, Value, ?MODULE, Vars).

get_server(Vars) ->
    ej_vars:get(server,?MODULE, Vars).
set_server(Value,Vars) ->
    ej_vars:set(server, Value, ?MODULE, Vars).

get_user(Vars) ->
    ej_vars:get(user,?MODULE, Vars).
set_user(Value,Vars) ->
    ej_vars:set(user, Value, ?MODULE, Vars).

get_lang(Vars) ->
    ej_vars:get(lang,?MODULE, Vars).
set_lang(Value,Vars) ->
    ej_vars:set(lang, Value, ?MODULE, Vars).

get_resource(Vars) ->
    ej_vars:get(resource,?MODULE, Vars).
set_resource(Value,Vars) ->
    ej_vars:set(resource, Value, ?MODULE, Vars).

get_jid(Vars) ->
    ej_vars:get(jid,?MODULE, Vars).
set_jid(Value,Vars) ->
    ej_vars:set(jid, Value, ?MODULE, Vars).

get_access(Vars) ->
    ej_vars:get(access,?MODULE, Vars).
set_access(Value,Vars) ->
    ej_vars:set(access, Value, ?MODULE, Vars).

get_sid(Vars) ->
    ej_vars:get(sid,?MODULE, Vars).
set_sid(Value,Vars) ->
    ej_vars:set(sid, Value, ?MODULE, Vars).

%% todo
stream_mgmt_enabled(_Vars) ->
    true.


change_state(State, Vars) ->
    %% OldState = ej_vars:get(ul_entity,?MODULE,Vars),
    %% do I really need to delete the old state to save memory?
    %% NewVars0 = ej_vars:delete(OldState, Vars),
    ct:log("~p~n",[ej_utils:sizeof(Vars)]),
    ej_vars:set(ul_entity, State, ?MODULE, Vars).


terminate(_Args, _Var) ->
    1.


%% Local Variables:
%% mode:erlang
%% coding: undecided-unix
%% End:
