-module(ej_c2s_state_wait_for_bind).
-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("sp_cmd.hrl").
-export([
          new/1,
          ul/2
        ]).


new(Vars) ->
    ej_vars:add_module(?MODULE, #{
                         }, Vars).

ul({xml_stream_element, El}, Vars) ->
    IqQueryInfo = jlib:iq_query_info(El),
    case IqQueryInfo of
        #iq{type = set, xmlns = ?NS_BIND, sub_el = SubEl} = IQ ->
            go_on_0(El, SubEl, IQ, Vars);
        _ ->
            %% on error, do nonthing, state at the same state.
            Vars
    end.

go_on_0(El, SubEl, IQ, Vars) ->
    R1 = xml:get_path_s(SubEl, [{elem, <<"resource">>}, cdata]),
    case jlib:resourceprep(R1) of
        error ->
            resource_err(El,Vars);
        <<"">> ->
            RandomResource = random_resource(),
            go_on_1(El, RandomResource, IQ, Vars);
        Resource ->
            go_on_1(El, Resource, IQ, Vars)
    end.

go_on_1(El, R, IQ,Vars) ->
    U = ej_c2s_state:get_user(Vars),
    Server = ej_c2s_state:get_server(Vars),
    case resource_conflict_action(U, Server, R) of
        closenew ->
            Err = jlib:make_error_reply(El,
                                        ?STANZA_ERROR(<<"409">>,
                                                      <<"modify">>,
                                                      <<"conflict">>)),
            %% stay the same state
            ej_c2s:dl({send_xml, [Err]}, ?MODULE, Vars);
        {accept_resource, R2} ->
            JID = jlib:make_jid(U, Server, R2),
            Res = IQ#iq{type = result,
                        sub_el =
                            [#xmlel{name = <<"bind">>,
                                    attrs = [{<<"xmlns">>, ?NS_BIND}],
                                    children =
                                        [#xmlel{name = <<"jid">>,
                                                attrs = [],
                                                children =
                                                    [{xmlcdata,
                                                      jlib:jid_to_string(JID)}]}]}]},
            Xml = jlib:iq_to_xml(Res),
            NewVars0 = ej_c2s:dl({send_xml, [Xml]}, ?MODULE, Vars),
            NewVars1 = ej_c2s_state:set_resource(R2, NewVars0),
            NewVars2 = ej_c2s_state:set_jid(JID, NewVars1),
            NewSID = {now(), ej_tcp_stub:get_socket(NewVars2)},
            NewVars3 = ej_c2s_state:set_sid(NewSID, NewVars2),
            ej_c2s_state:change_state(ej_c2s_state_wait_for_session, NewVars3)
    end.

resource_err(El, Vars) ->
    Err = jlib:make_error_reply(El, ?ERR_BAD_REQUEST),
    %% stay in the same state, so that no need to change state.
    ej_c2s:dl({send_xml, [Err]}, ?MODULE, Vars).

random_resource() ->
    iolist_to_binary([randoms:get_string()
                      | [jlib:integer_to_binary(X)
                         || X <- tuple_to_list(now())]]).

resource_conflict_action(U, S, R) ->
    OptionRaw = case ejabberd_sm:is_existing_resource(U, S, R) of
                  true ->
                      ejabberd_config:get_option(
                        {resource_conflict, S},
                        fun(setresource) -> setresource;
                           (closeold) -> closeold;
                           (closenew) -> closenew;
                           (acceptnew) -> acceptnew
                        end);
                  false ->
                      acceptnew
                end,
    Option = case OptionRaw of
               setresource -> setresource;
               closeold ->
                   acceptnew; %% ejabberd_sm will close old session
               closenew -> closenew;
               acceptnew -> acceptnew;
               _ -> acceptnew %% default ejabberd behavior
             end,
    case Option of
      acceptnew -> {accept_resource, R};
      closenew -> closenew;
      setresource ->
          Rnew = iolist_to_binary([randoms:get_string()
                                   | [jlib:integer_to_binary(X)
                                      || X <- tuple_to_list(now())]]),
          {accept_resource, Rnew}
    end.
