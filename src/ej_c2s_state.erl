%%%-------------------------------------------------------------------
%%% @author chunywan <wcy123@gmail.com>
%%% @copyright (C) 2015, chunywan
%%% @doc
%%%
%%% @end
%%% Created :  4 Aug 2015 by chunywan <wcy123@gmail.com>
%%%-------------------------------------------------------------------
-module(ej_c2s_state).
-include("sp_cmd.hrl").
-include("jlib.hrl").


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
         get_pres_f/1,
         set_pres_f/2,
         get_pres_t/1,
         set_pres_t/2,
         get_privacy_list/1,
         set_privacy_list/2,
         stream_mgmt_enabled/1
        ]).
-spec new(Vars :: ej_vars:ej_vars()) -> ej_vars:ej_vars().
new(Vars) ->
    %% maybe I need to initialize all the states here.
    M = #{
      ul_entity => undefined,
      detail_level => 9,
      dl_entity => ?MODULE
     },
    States = [
              %% all states go here
              ej_c2s_state_wait_for_stream,
              ej_c2s_state_wait_for_feature_request,
              ej_c2s_state_wait_for_bind,
              ej_c2s_state_wait_for_session,
              ej_c2s_state_session_established
             ],
    NewVars0 = lists:foldl(fun (State, V) -> maps:put(State, M, V) end, Vars, States),
    NewVars1 = ej_vars:new(States, NewVars0),
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
                          pres_f => undefined,
                          pres_t => undefined,
                          privacy_list => undefined,
                          %% the stream id
                          stream_id => randoms:get_string()
                        },
                       NewVars1).
-spec ul(Cmd::#sp_cmd{},Vars::ej_vars:ej_vars()) -> ej_vars:ej_vars().
ul(Args, Vars) ->
    ej_c2s:ul(Args,?MODULE,Vars).

-spec dl(Cmd::#sp_cmd{},Vars::ej_vars:ej_vars()) -> ej_vars:ej_vars().
dl(#sp_cmd{args = {send_xml, _XMLs} } = Cmd, Vars) ->
    ej_c2s:dl(Cmd,?MODULE, Vars).

-spec get_stream_id(Vars :: ej_vars:ej_vars()) ->  binary() | undefined.
get_stream_id(Vars) ->
    ej_vars:get(stream_id,?MODULE, Vars).
-spec set_stream_id(Value :: binary() | undefined, Vars :: ej_vars:ej_vars() ) -> ej_vars:ej_vars().
set_stream_id(Value,Vars) ->
    ej_vars:set(stream_id, Value, ?MODULE, Vars).

-spec get_server(Vars :: ej_vars:ej_vars()) ->  binary().
get_server(Vars) ->
    ej_vars:get(server,?MODULE, Vars).
-spec set_server(Value :: binary(), Vars :: ej_vars:ej_vars() ) -> ej_vars:ej_vars().
set_server(Value,Vars) ->
    ej_vars:set(server, Value, ?MODULE, Vars).


-spec get_user(Vars :: ej_vars:ej_vars()) ->  binary().
get_user(Vars) ->
    ej_vars:get(user,?MODULE, Vars).
-spec set_user(Value :: binary(), Vars :: ej_vars:ej_vars() ) -> ej_vars:ej_vars().
set_user(Value,Vars) ->
    ej_vars:set(user, Value, ?MODULE, Vars).

-spec get_lang(Vars :: ej_vars:ej_vars()) ->  binary().
get_lang(Vars) ->
    ej_vars:get(lang,?MODULE, Vars).
-spec set_lang(Value :: binary(), Vars :: ej_vars:ej_vars() ) -> ej_vars:ej_vars().
set_lang(Value,Vars) ->
    ej_vars:set(lang, Value, ?MODULE, Vars).

-spec get_resource(Vars :: ej_vars:ej_vars()) ->  binary().
get_resource(Vars) ->
    ej_vars:get(resource,?MODULE, Vars).
-spec set_resource(Value :: binary(), Vars :: ej_vars:ej_vars() ) -> ej_vars:ej_vars().
set_resource(Value,Vars) ->
    ej_vars:set(resource, Value, ?MODULE, Vars).

-spec get_jid(Vars :: ej_vars:ej_vars()) ->  #jid{} | undefined.
get_jid(Vars) ->
    ej_vars:get(jid,?MODULE, Vars).
-spec set_jid(Value :: #jid{} , Vars :: ej_vars:ej_vars() ) -> ej_vars:ej_vars().
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

get_pres_f(Vars) ->
    ej_vars:get(pres_f,?MODULE, Vars).
set_pres_f(Value,Vars) ->
    ej_vars:set(pres_f, Value, ?MODULE, Vars).
get_pres_t(Vars) ->
    ej_vars:get(pres_t,?MODULE, Vars).
set_pres_t(Value,Vars) ->
    ej_vars:set(pres_t, Value, ?MODULE, Vars).
get_privacy_list(Vars) ->
    ej_vars:get(privacy_list,?MODULE, Vars).
set_privacy_list(Value,Vars) ->
    ej_vars:set(privacy_list, Value, ?MODULE, Vars).

%% todo
-spec stream_mgmt_enabled(ej_vars:ej_vars()) -> boolean().
stream_mgmt_enabled(#{}) ->
    false;
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
