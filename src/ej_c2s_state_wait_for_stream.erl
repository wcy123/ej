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

                         }, Vars).
ul({xml_stream_start, _Name, Attrs}, Vars) ->
    DefaultLang = ?MYLANG,
    case xml:get_attr_s(<<"xmlns:stream">>, Attrs) of
        ?NS_STREAM ->
            Vars;
        _ ->
            invalid_ns_err(DefaultLang, <<"">>, ?MYNAME, Vars)
    end.




%%%===================================================================
%%% Internal functions
%%%===================================================================


invalid_ns_err(Lang, Version, Server, Vars) ->
    NewVars0 = ej_c2s:dl({send_xml,
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
                 Vars),
    NewVars0.
