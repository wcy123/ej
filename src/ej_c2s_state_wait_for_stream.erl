%%%-------------------------------------------------------------------
%%% @author wcy123 <wcy123@gmail.com>
%%% @copyright (C) 2015, UML public
%%% @doc
%%%
%%% @end
%%% Created : 14 Aug 2015 by Wang Chunye <>
%%%-------------------------------------------------------------------
-module(ej_c2s_state_wait_for_stream).
-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("sp_cmd.hrl").


%% API
-export([
         new/1,
         ul/2,
         get_sasl_state/1,
         set_sasl_state/2,
         set_authenticated/2,
         get_auth_module/1,
         set_auth_module/2
        ]).

%%% MACRORS
-define(INVALID_NS_ERR, ?SERR_INVALID_NAMESPACE).
-define(HOST_UNKNOWN_ERR, ?SERR_HOST_UNKNOWN).
-define(POLICY_VIOLATION_ERR(Lang, Text),
        ?SERRT_POLICY_VIOLATION(Lang, Text)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

new(Vars) ->
    ej_vars:add_module(?MODULE, #{
                          authenticated => false,
                          sasl_state => undefined,
                          auth_module => undefined,
                          output => undefined
                         }, Vars).
ul({xml_stream_start, _Name, Attrs}, Vars) ->
    DefaultLang = ?MYLANG,
    case xml:get_attr_s(<<"xmlns:stream">>, Attrs) of
        ?NS_STREAM ->
            go_on_0(Attrs,Vars);
        _ ->
            invalid_ns_err(DefaultLang, <<"">>, ?MYNAME, Vars)
    end.

go_on_0(Attrs, Vars) ->
    Server = get_server(Attrs, Vars),
    Lang = get_lang(Attrs),
    NewVars = ej_c2s_state:set_lang(Lang, Vars),
    case lists:member(Server, ?MYHOSTS) of
        true -> go_on_1(Lang, Server, Attrs, NewVars);
        false -> host_unknown_err(NewVars)
    end.

go_on_1(Lang, Server, Attrs, Vars) ->
    IsBlacklistedIP = is_ip_blacklisted(ej_tcp_stub:get_ip(Vars), Lang),
    case IsBlacklistedIP of
        false -> go_on_2(Lang,Server,Attrs,Vars);
        {true,_,_} -> black_list_err(IsBlacklistedIP, Lang, Vars)
    end.

go_on_2(Lang, Server, Attrs, Vars) ->
    %% TODO: consider to implement this feature in the lower protocol entity
    %% for example:
    %%      ej_tcp_stub:change_shaper().
    %% change_shaper(StateData, jlib:make_jid(<<"">>, Server, <<"">>)),
    %%%
    Version = xml:get_attr_s(<<"version">>, Attrs),
    case Version of
        <<"1.0">> ->
            go_on_3(Lang,Server,Attrs,Vars);
        _ ->
            not_version_1_0_err(Vars)
    end.

go_on_3(Lang,Server,Attrs,Vars) ->
    case get_authenticated(Vars) of
        false ->
            go_on_not_authenticated(Server,Vars);
        true ->
            go_on_authenticated(Server,Lang,Vars)
    end.

go_on_not_authenticated(Server, Vars) ->
    DefaultLang = ?MYLANG,
    SASLState =
        cyrsasl:server_new(
          <<"jabber">>, Server, <<"">>, [],
          fun(U) ->
                  ejabberd_auth:get_password_with_authmodule(
                    U, Server)
          end,
          fun(U, P) ->
                  ejabberd_auth:check_password_with_authmodule(
                    U, Server, P)
          end,
          fun(U, P, D, DG) ->
                  ejabberd_auth:check_password_with_authmodule(
                    U, Server, P, D, DG)
          end),
    Ms = lists:map(fun (S) ->
                           #xmlel{name = <<"mechanism">>,
                                  attrs = [],
                                  children = [{xmlcdata, S}]}
                   end,
                   cyrsasl:listmech(Server)),
    Mechs = [#xmlel{name = <<"mechanisms">>,
                    attrs = [{<<"xmlns">>, ?NS_SASL}],
                    children = Ms}],
    CompressFeature = [],
    TLSFeature  = [],
    StreamFeatures1 = TLSFeature ++ CompressFeature ++ Mechs,
    StreamFeatures = ejabberd_hooks:run_fold(c2s_stream_features,
                                             Server, StreamFeatures1, [Server]),
    Version = <<"1.0">>,
    NewVars0 = set_sasl_state(SASLState, Vars),
    NewVars1 = ej_c2s_state:set_server(Server, NewVars0),
    NewVars2 = ej_c2s:dl(
                 {send_xml,
                 [xml_1_0,
                  { xml_stream_start,
                    %% name =
                    <<"stream:stream">>,
                    %% attrs =
                    [{ <<"versiaon">>, Version},
                     {<<"xml:lang">>, DefaultLang},
                     {<<"xmlns">>, <<"jabber:client">>},
                     {<<"xmlns:stream">>, <<"http://etherx.jabber.org/streams">>},
                     {<<"id">>, ej_c2s_state:get_stream_id(Vars)},
                     {<<"from">>, Server} ]},
                  #xmlel{ name = <<"stream:features">>,
                          attrs = [],
                          children = StreamFeatures }
                 ]},
                 ?MODULE,
                 NewVars1),
    ej_c2s_state:change_state(ej_c2s_state_wait_for_feature_request,NewVars2).

go_on_authenticated(Server, Lang, Vars) ->
    case ej_c2s_state:get_resource(Vars) of
        <<"">> ->
            go_on_empty_resource(Server, Lang, Vars);
        _ ->
            go_on_nonempty_resource(Server, Lang, Vars)
    end.

go_on_empty_resource(Server,Lang,Vars) ->
    RosterVersioningFeature =
        ejabberd_hooks:run_fold(roster_get_versioning_feature,
                                Server, [],
                                [Server]),
    StreamManagementFeature =
        case ej_c2s_state:stream_mgmt_enabled(Vars) of
            true ->
                [#xmlel{name = <<"sm">>,
                        attrs = [{<<"xmlns">>, ?NS_STREAM_MGMT_2}],
                        children = []},
                 #xmlel{name = <<"sm">>,
                        attrs = [{<<"xmlns">>, ?NS_STREAM_MGMT_3}],
                        children = []}];
            false ->
                []
        end,
    StreamFeatures1 = [#xmlel{name = <<"bind">>,
                              attrs = [{<<"xmlns">>, ?NS_BIND}],
                              children = []},
                       #xmlel{name = <<"session">>,
                              attrs = [{<<"xmlns">>, ?NS_SESSION}],
                              children = []}]
        ++
        RosterVersioningFeature ++
        StreamManagementFeature ++
        ejabberd_hooks:run_fold(c2s_post_auth_features,
                                Server, [], [Server]),
    StreamFeatures = ejabberd_hooks:run_fold(c2s_stream_features,
                                             Server, StreamFeatures1, [Server]),
    NewVars0 = ej_c2s:dl(
                {
                  send_xml,
                  [ #xmlel{name = <<"stream:features">>,
                           attrs = [],
                           children = StreamFeatures} ]
                }, ?MODULE, Vars),
    NewVars1 = ej_c2s_state:set_lang(Lang, NewVars0),
    NewVars2 = ej_c2s_state:set_server(Server, NewVars1),
    ej_c2s_state:change_state(ej_c2s_state_wait_for_bind, NewVars2).

go_on_nonempty_resource(Server,Lang,Vars) ->
    NewVars0 = ej_c2s:dl(
                {
                  send_xml,
                  [#xmlel{name = <<"stream:features">>,
                          attrs = [],
                          children = []}]
                }, ?MODULE, Vars),
    NewVars1 = ej_c2s_state:set_lang(Lang, NewVars0),
    NewVars2 = ej_c2s_state:set_server(Server, NewVars1),
    ej_c2s_state:change_state(ej_c2s_state_wait_for_session, NewVars2).

get_server(Attrs, Vars) ->
    case ej_c2s_state:get_server(Vars) of
        <<"">> ->
            jlib:nameprep(xml:get_attr_s(<<"to">>, Attrs));
        S -> S
    end.

get_lang(Attrs) ->
    case xml:get_attr_s(<<"xml:lang">>, Attrs) of
        Lang1 when byte_size(Lang1) =< 35 ->
            %% As stated in BCP47, 4.4.1:
            %% Protocols or specifications that
            %% specify limited buffer sizes for
            %% language tags MUST allow for
            %% language tags of at least 35 characters.
            Lang1;
        _ ->
            %% Do not store long language tag to
            %% avoid possible DoS/flood attacks
            <<"">>
    end.



%%%===================================================================
%%% Internal functions
%%%===================================================================


invalid_ns_err(Lang, Version, Server, Vars) ->
    ej_c2s:dl({send_xml,
               [xml_1_0,
                #xmlel{
                   name  = <<"stream:stream">>,
                   attrs = [{ <<"version">>, Version},
                            {<<"xml:lang">>, Lang},
                            {<<"xmlns">>, <<"jabber:client">>},
                            {<<"xmlns:stream">>, <<"http://etherx.jabber.org/streams">>},
                            {<<"id">>, ej_c2s_state:get_stream_id(Vars)},
                            {<<"from">>, Server} ],
                   children = [
                               ?INVALID_NS_ERR
                              ]
                  }
               ]},
              ?MODULE,
              Vars).

host_unknown_err(Vars) ->
    DefaultLang = ?MYLANG,
    ej_c2s:dl({send_xml,
               [xml_1_0,
                #xmlel{
                   name  = <<"stream:stream">>,
                   attrs = [{ <<"version">>, <<"">>},
                            {<<"xml:lang">>, DefaultLang},
                            {<<"xmlns">>, <<"jabber:client">>},
                            {<<"xmlns:stream">>, <<"http://etherx.jabber.org/streams">>},
                            {<<"id">>, ej_c2s_state:get_stream_id(Vars)},
                            {<<"from">>, ?MYNAME } ],
                   children = [
                               ?HOST_UNKNOWN_ERR
                              ]
                  }
               ]},
              ?MODULE,
              Vars).

black_list_err(IsBlacklistedIP, Lang, Vars) ->
    DefaultLang = ?MYLANG,
    IP = ej_tcp_stub:get_ip(Vars),
    {true, LogReason, ReasonT} = IsBlacklistedIP,
    ?INFO_MSG("Connection attempt from blacklisted IP ~s: ~s",
              [jlib:ip_to_list(IP), LogReason]),
    From = ?MYNAME,
    Child = ?POLICY_VIOLATION_ERR(Lang, ReasonT),
    ej_c2s:dl({send_xml,
               [xml_1_0,
                #xmlel{
                   name  = <<"stream:stream">>,
                   attrs = [{ <<"version">>, <<"">>},
                            {<<"xml:lang">>, DefaultLang},
                            {<<"xmlns">>, <<"jabber:client">>},
                            {<<"xmlns:stream">>, <<"http://etherx.jabber.org/streams">>},
                            {<<"id">>, ej_c2s_state:get_stream_id(Vars)},
                            {<<"from">>, From} ],
                   children = [Child]
                  }
               ]}, ?MODULE, Vars).

not_version_1_0_err(Vars) ->
    DefaultLang = ?MYLANG,
    From = ?MYNAME,
    Child = ?POLICY_VIOLATION_ERR(DefaultLang, <<"wrong version">>),
    ej_c2s:dl({send_xml,
               [xml_1_0,
                #xmlel{
                   name  = <<"stream:stream">>,
                   attrs = [{ <<"version">>, <<"">>},
                            {<<"xml:lang">>, DefaultLang},
                            {<<"xmlns">>, <<"jabber:client">>},
                            {<<"xmlns:stream">>, <<"http://etherx.jabber.org/streams">>},
                            {<<"id">>, ej_c2s_state:get_stream_id(Vars)},
                            {<<"from">>, From} ],
                   children = [Child]
                  }
               ]}, ?MODULE, Vars).

    %% if not StateData#state.tls_enabled and
    %%    StateData#state.tls_required ->
    %%         send_element(StateData,
    %%                      ?POLICY_VIOLATION_ERR(Lang,
    %%                                            <<"Use of STARTTLS required">>)),
    %%         send_trailer(StateData),
    %%         {stop, normal, StateData};
    %%    true ->
    %%         fsm_next_state(wait_for_auth,
    %%                        StateData#state{server = Server,
    %%                                        lang = Lang})
    %% end



%% Used by c2s blacklist plugins
is_ip_blacklisted(undefined, _Lang) -> false;
is_ip_blacklisted({IP, _Port}, Lang) ->
    ejabberd_hooks:run_fold(check_bl_c2s, false, [IP, Lang]).

get_authenticated(Vars) ->
    ej_vars:get(authenticated, ?MODULE, Vars).
set_authenticated(Value, Vars) ->
    ej_vars:set(authenticated, Value, ?MODULE, Vars).


get_sasl_state(Vars) ->
    ej_vars:get(sasl_state,?MODULE, Vars).
set_sasl_state(Value,Vars) ->
    ej_vars:set(sasl_state, Value, ?MODULE, Vars).

get_auth_module(Vars) ->
    ej_vars:get(auth_module,?MODULE, Vars).
set_auth_module(Value,Vars) ->
    ej_vars:set(auth_module, Value, ?MODULE, Vars).
