-module(ej_c2s_state_wait_for_session).
-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").
-include("sp_cmd.hrl").
-export([
         new/1,
         ul/2
        ]).

-define(SETS, gb_sets).
-define(DICT, dict).

-spec new(Vars :: ej_vars:ej_vars()) -> ej_vars:ej_vars().
new(Vars) ->
    ej_vars:add_module(?MODULE, #{
                         }, Vars).

-spec ul(Cmd::#sp_cmd{},Vars::ej_vars:ej_vars()) -> ej_vars:ej_vars().
ul(#sp_cmd{ args = {xml_stream_element, El} }, Vars) ->
    %% this one is not migrate yet.
    %% NewStateData = update_num_stanzas_in(StateData, El),
    IqQueryInfo = jlib:iq_query_info(El),
    case IqQueryInfo of
        #iq{type = set, xmlns = ?NS_SESSION} ->
            go_on_0(El,Vars);
        _ ->
            %% on error, do nonthing, state at the same state.
            Vars
    end.

go_on_0(El,Vars) ->
    Server = ej_c2s_state:get_server(Vars),
    Access = ej_c2s_state:get_access(Vars),
    JID = ej_c2s_state:get_jid(Vars),
    case acl:match_rule(Server, Access , JID) of
        allow -> go_on_allow(El,Vars);
        _ -> go_on_not_allow(El,Vars)
    end.

go_on_allow(El,Vars) ->
    U = ej_c2s_state:get_user(Vars),
    R = ej_c2s_state:get_resource(Vars),
    JID = ej_c2s_state:get_jid(Vars),
    Server = ej_c2s_state:get_server(Vars),
    Socket = ej_tcp_stub:get_socket(Vars),
    ?INFO_MSG("(~w) Opened session for ~s", [Socket, jlib:jid_to_string(JID)]),
    Res = jlib:make_result_iq_reply(El#xmlel{children = []}),
    NewVars0 = ej_c2s:dl(
                 #sp_cmd{
                    args = {send_xml, [Res]},
                    label = <<"open session">>
                   },
                 ?MODULE, Vars),
    %% this feature could be implemented in a more modular way.
    %% CSI etc
    %% send_stanza(Res, Vars),
    NewVars1 = ej_tcp_stub:change_shaper(JID,NewVars0),
    {Fs, Ts} = ejabberd_hooks:run_fold(
                 roster_get_subscription_lists,
                 Server,
                 {[], []},
                 [U, Server]),
    LJID = jlib:jid_tolower(jlib:jid_remove_resource(JID)),
    Fs1 = [LJID | Fs],
    Ts1 = [LJID | Ts],
    PrivList =
        ejabberd_hooks:run_fold(
          privacy_get_user_list, Server,
          #userlist{},
          [U, Server]),
    Conn = get_conn_type(Vars),
    Info = [{ip, ej_tcp_stub:get_ip(Vars)}, {conn, Conn},
            {auth_module, ej_c2s_state_wait_for_stream:get_auth_module(Vars)}],
    ejabberd_sm:open_session(
      ej_c2s_state:get_sid(Vars), U, Server, R, Info),
    NewVars2 = ej_c2s_state:set_pres_f(?SETS:from_list(Fs1), NewVars1),
    NewVars3 = ej_c2s_state:set_pres_t(?SETS:from_list(Ts1), NewVars2),
    NewVars4 = ej_c2s_state:set_privacy_list(PrivList, NewVars3),
    ej_c2s_state:change_state(ej_c2s_state_session_established, NewVars4).


go_on_not_allow(El,Vars) ->
    Server = ej_c2s_state:get_server(Vars),
    Socket = ej_tcp_stub:get_socket(Vars),
    JID = ej_c2s_state:get_jid(Vars),
    ejabberd_hooks:run(forbidden_session_hook, Server, [JID]),
    ?INFO_MSG("(~w) Forbidden session for ~s", [Socket, jlib:jid_to_string(JID)]),
    Err = jlib:make_error_reply(El, ?ERR_NOT_ALLOWED),
    NewVars0 = ej_c2s:dl(
                 #sp_cmd{
                    args = {send_xml, [Err]},
                    label = <<"session denied">>
                   },
                 ?MODULE, Vars),
    %% this has no effect, stay in the same state.
    ej_c2s_state:change_state(ej_c2s_state_wait_for_session, NewVars0).

%% update_num_stanzas_in(#state{mgmt_state = active} = StateData, El) ->
%%     NewNum = case {is_stanza(El), StateData#state.mgmt_stanzas_in} of
%%                {true, 4294967295} ->
%%                    0;
%%                {true, Num} ->
%%                    Num + 1;
%%                {false, Num} ->
%%                    Num
%%              end,
%%     StateData#state{mgmt_stanzas_in = NewNum};
%% update_num_stanzas_in(StateData, _El) ->
%%     StateData.


get_conn_type(Vars) ->
    ej_tcp_stub:get_conn_type(Vars).
