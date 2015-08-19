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


%% API
-export([
         new/1,
         ul/2
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
                          server => <<"">>,
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
    case lists:member(Server, ?MYHOSTS) of
        true -> go_on_1(Lang, Server, Attrs, Vars);
        false -> host_unknown_err(Vars)
    end.

go_on_1(Lang, Server, Attrs, Vars) ->
    IsBlacklistedIP = is_ip_blacklisted(ej_tcp_stub:get_ip(Vars), Lang),
    case IsBlacklistedIP of
        false -> go_on_2(Lang,Server,Attrs,Vars);
        {true,_,_} -> black_list_err(IsBlacklistedIP, Lang, Vars)
    end.

go_on_2(Lang, Server, Attrs, Vars) ->
    error({hi_todo, Lang, Server, Attrs}),
    Vars.

get_server(Attrs, Vars) ->
    case ej_vars:get(server, ?MODULE, Vars) of
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
    From = ?MYLANG,
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




%% Used by c2s blacklist plugins
is_ip_blacklisted(undefined, _Lang) -> false;
is_ip_blacklisted({IP, _Port}, Lang) ->
    ejabberd_hooks:run_fold(check_bl_c2s, false, [IP, Lang]).
