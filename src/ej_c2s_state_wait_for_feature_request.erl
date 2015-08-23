%%%-------------------------------------------------------------------
%%% @author wcy123 <wcy123@gmail.com>
%%% @copyright (C) 2015, UML public
%%% @doc
%%%
%%% @end
%%% Created : 14 Aug 2015 by Wang Chunye <>
%%%-------------------------------------------------------------------
-module(ej_c2s_state_wait_for_feature_request).
-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("sp_cmd.hrl").
%% API
-export([
         new/1,
         ul/2
        ]).
%%% MACRORS

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

-spec new(Vars :: ej_vars:ej_vars()) -> ej_vars:ej_vars().
new(Vars) ->
    ej_vars:add_module(?MODULE, #{

                         }, Vars).
-spec ul(Cmd::#sp_cmd{},Vars::ej_vars:ej_vars()) -> ej_vars:ej_vars().
ul(#sp_cmd{ args = {xml_stream_element, El} } , Vars) ->
    #xmlel{name = Name, attrs = Attrs, children = Els} = El,
    NS = xml:get_attr_s(<<"xmlns">>, Attrs),
    case {NS , Name} of
        {?NS_SASL, <<"auth">>} -> go_on_auth(Els, Attrs, Vars);
        {?NS_TLS, <<"starttls">>} -> error(todo);
        {?NS_COMPRESS, <<"compress">>} -> error(todo);
        _ ->
            %% todo <<"Use of STARTTLS required">>
            go_on_unknown_ns(El,Vars)
    end.


go_on_auth(Els, Attrs, Vars) ->
    Mech = xml:get_attr_s(<<"mechanism">>, Attrs),
    ClientIn = jlib:decode_base64(xml:get_cdata(Els)),
    case cyrsasl:server_start(
           ej_c2s_state_wait_for_stream:get_sasl_state(Vars),
           Mech, ClientIn) of
        {ok, Props} ->
            go_on_auth_ok(Props, Vars);
        %% {continue, ServerOut, NewSASLState} ->
        %%     send_element(StateData,
        %%                  #xmlel{name = <<"challenge">>,
        %%                         attrs = [{<<"xmlns">>, ?NS_SASL}],
        %%                         children =
        %%                             [{xmlcdata,
        %%                               jlib:encode_base64(ServerOut)}]}),
        %%     fsm_next_state(wait_for_sasl_response,
        %%                    StateData#state{sasl_state = NewSASLState});
        {error, Error, Username} ->
            go_on_auth_failed(Username, Error, Vars)
            %%                         attrs = [{<<"xmlns">>, ?NS_SASL}],
            %%                         children =
            %%                             [#xmlel{name = Error, attrs = [],
            %%                                     children = []}]}),
            %%     fsm_next_state(wait_for_feature_request, StateData)
    end.
go_on_auth_ok(Props, Vars) ->
    U = proplists:get_value(username, Props, <<>>),
    AuthModule = proplists:get_value(auth_module, Props, undefined),
    ?INFO_MSG("(~w) Accepted authentication for ~s "
              "by ~p from ~s",
              [ ej_tcp_stub:get_socket(Vars), U, AuthModule,
                ejabberd_config:may_hide_data(jlib:ip_to_list(ej_tcp_stub:get_ip(Vars)))]),
    ejabberd_hooks:run(c2s_auth_result, ej_c2s_state:get_server(Vars),
                       [true, U, ej_c2s_state:get_server(Vars),
                        ej_tcp_stub:get_ip(Vars)]),
    NewVars0 = ej_c2s:dl(
                 #sp_cmd{
                    args = {send_xml,
                            [
                             #xmlel{name = <<"success">>,
                                    attrs = [{<<"xmlns">>, ?NS_SASL}],
                                    children = []}
                            ]
                           },
                    label = <<"success">>
                   },
                 ?MODULE, Vars),
    NewVars1 = ej_c2s_state:set_stream_id(new_id(),NewVars0),
    NewVars2 = ej_c2s_state_wait_for_stream:set_authenticated(true,NewVars1),
    NewVars3 = ej_c2s_state_wait_for_stream:set_auth_module(AuthModule,NewVars2),
    NewVars4 = ej_c2s_state_wait_for_stream:set_sasl_state(undefined,NewVars3),
    NewVars5 = ej_c2s_state:set_user(U,NewVars4),
    NewVars6 = ej_c2s_state:change_state(ej_c2s_state_wait_for_stream,NewVars5),
    NewVars7 = ej_xml_stream:reset_stream(NewVars6),
    NewVars7.

go_on_auth_failed(Username, Error, Vars) ->
    Server = ej_c2s_state:get_server(Vars),
    IP = ej_tcp_stub:get_ip(Vars),
    ?INFO_MSG("(~w) Failed authentication for ~s@~s from ~s",
              [ ej_tcp_stub:get_socket(Vars),
                Username, Server,
                ejabberd_config:may_hide_data(jlib:ip_to_list(IP))]),
    ejabberd_hooks:run(c2s_auth_result, ej_c2s_state:get_server(Vars),
                       [false, Username, Server, IP]),
    NewVars = ej_c2s:dl(
                #sp_cmd{
                   args = {
                     send_xml,
                     [#xmlel{name = <<"failure">>,
                             attrs = [{<<"xmlns">>, ?NS_SASL}],
                             children =
                                 [#xmlel{name = Error, attrs = [],
                                         children = []}]}]
                    },
                   label = <<"failure">>
                  },
                ?MODULE, Vars),
    NewVars.
go_on_unknown_ns(El, Vars) ->
    NewVars = process_unauthenticated_stanza(El,Vars),
    ej_c2s_state:change_state(wait_for_feature_request, NewVars).

process_unauthenticated_stanza(El,Vars) ->
    NewEl = case xml:get_tag_attr_s(<<"xml:lang">>, El) of
                <<"">> ->
                    case ej_c2s_state:get_lang(Vars) of
                        <<"">> -> El;
                        Lang -> xml:replace_tag_attr(<<"xml:lang">>, Lang, El)
                    end;
                _ -> El
            end,
    Server = ej_c2s_state:get_server(Vars),
    IP = ej_tcp_stub:get_ip(Vars),
    case jlib:iq_query_info(NewEl) of
        #iq{} = IQ ->
            Res = ejabberd_hooks:run_fold(c2s_unauthenticated_iq,
                                          Server, empty,
                                          [Server, IQ, IP]),
            case Res of
                empty ->
                    ResIQ = IQ#iq{type = error,
                                  sub_el = [?ERR_SERVICE_UNAVAILABLE]},
                    Res1 = jlib:replace_from_to(jlib:make_jid(<<"">>,
                                                              Server,
                                                              <<"">>),
                                                jlib:make_jid(<<"">>, <<"">>,
                                                              <<"">>),
                                                jlib:iq_to_xml(ResIQ)),
                    XmlEl = jlib:remove_attr(<<"to">>, Res1),
                    ej_c2s:dl(
                      #sp_cmd{
                         args = {send_xml, [XmlEl]},
                         label = <<"service_not_available">>
                        },
                      ?MODULE, Vars);
                Otherwise ->
                    ej_c2s:dl(
                      #sp_cmd{
                         args = {send_xml, [Otherwise]},
                         label = <<"other_error">>
                        },
                      ?MODULE, Vars)
            end;
        _ ->
            %% Drop any stanza, which isn't IQ stanza
            Vars
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
new_id() -> randoms:get_string().
