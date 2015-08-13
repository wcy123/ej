%%%-------------------------------------------------------------------
%%% @author chunywan <wcy123@gmail.com>
%%% @copyright (C) 2015, chunywan
%%% @doc
%%%
%%% @end
%%% Created :  4 Aug 2015 by chunywan <wcy123@gmail.com>
%%%-------------------------------------------------------------------
-module(ej_c2s_raw_transport).



%% API

%% callbacks for upper layer
-export([send/2]).
%% ej_c2s_main_loop callbacks
-export([
         init/1,
         terminate/2,
         handle_info/2
        ]).
-record( state, {
           socket, %% tcp socket
           xml_stream, %% state returned by xml_stream:new
           upper_layer %% upper layer state.
          }).
-record(xml_stream_state,
	{
          port                  :: port(),
          stack = []            :: stack(),
          size = 0              :: non_neg_integer(),
          maxsize = infinity    :: non_neg_integer() | infinity
        }).
-define(DETAIL_LEVEL,50).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc
%%
%%   send an xml element to the stream asynchronously, always return
%%   ok. errors lead to crash.
%%
-spec send(gen_tcp:socket(), xml_stream:xml_stream_el()) -> ok.
send(Socket, XmlEls)                    ->
    Data = lists:map(fun element_to_binary/1, XmlEls),
    et:trace_me(?DETAIL_LEVEL, xml_stream, tcp, send_data , [{data,Data}]),
    ok = gen_tcp:send(Socket, Data).

%%%===================================================================
%%% callbacks
%%%===================================================================

init(Socket)                            ->
    et:trace_me(?DETAIL_LEVEL, xml_stream, c2s, init , []),
    {ok, C2sState} = ej_c2s:init(Socket, ?MODULE),
    XmlStream = xml_stream_new(infinity),
    %% now the ej_port_gc owns the port, so that when this processs is
    %% ended, the port still survive and keeps open. whenever a
    %% temporary process aborts, ej_port_gc will be notified.
    true = erlang:port_connect(XmlStream#xml_stream_state.port,
                               whereis(ej_port_gc)),
    State = #state{socket=Socket,
                   xml_stream = XmlStream,
                   upper_layer = C2sState
                  },
    et:trace_me(?DETAIL_LEVEL, xml_stream, main, ok , []),
    {ok, State}.
handle_info({tcp, Socket, Data}, State) ->
    XmlStream1 = State#state.xml_stream,
    true = erlang:link(XmlStream1#xml_stream_state.port),
    et:trace_me(?DETAIL_LEVEL, xml_stream, expal, parse , [{data, Data}]),
    {ok, XmlStream2, Events}  = parse(XmlStream1,Data),
    et:trace_me(?DETAIL_LEVEL, expal, xml_stream, ok , [{xml, XmlStream2}, {events, Events}]),
    {OK, NewUpperLayer}
        = lists:foldl(
            fun ej_c2s:handle_event/2,
            {ok, State#state.upper_layer},
            Events),
    NewState = State#state{
                 socket=Socket,
                 xml_stream = XmlStream2,
                 upper_layer = NewUpperLayer
                },
    return({OK,NewState}).
terminate(Socket,State)                 ->
    XmlStream = State#state.xml_stream,
    true = erlang:link(XmlStream#xml_stream_state.port),
    true = ej_xml_stream:close(XmlStream),
    {OK, NewUpperLayer} =  ej_c2s:handle_event({tcp_closed,Socket}, {ok, State#state.upper_layer}),
    return({OK, State#state{socket=Socket, upper_layer = NewUpperLayer}}).

%% on_error(_Pid, Socket, State)           ->
%%     XmlStream = State#state.xml_stream,
%%     ej_xml_stream:close(XmlStream),
%%     gen_tcp:close(Socket).

%%%===================================================================
%%% Internal functions
%%%===================================================================
return({OK, NewState}) ->
    case OK of
        ok -> {ok, NewState};
        _ ->
            ok = gen_tcp:close(NewState#state.socket),
            %% return anything else other ok, the resource is delete
            stop
    end.

%% from xml_stream
-define(XML_START, 0).
-define(XML_END, 1).
-define(XML_CDATA, 2).
-define(XML_ERROR, 3).
-define(PARSE_COMMAND, 0).
-define(PARSE_FINAL_COMMAND, 1).
-record(xmlel,
{
    name = <<"">> :: binary(),
    attrs    = [] :: [attr()],
    children = [] :: [xmlel() | cdata()]
}).

-type(cdata() :: {xmlcdata, CData::binary()}).
-type(attr() :: {Name::binary(), Value::binary()}).
-type(xmlel() :: #xmlel{}).
-type xml_stream_el() :: {xml_stream_raw, binary()} |
                         {xml_stream_cdata, binary()} |
                         {xml_stream_element, xmlel()} |
                         {xml_stream_end, binary()} |
                         {xml_stream_start, binary(), [attr()]} |
                         {xml_stream_error, binary()}.
%% it is wrong, it is not `xml_stream_el`
-type stack() :: [xml_stream_el() | xmlel()].
-type xml_stream_state() :: #xml_stream_state{}.

element_to_binary(xml_1_0) ->
    << "<?xml version='1.0'?>" >>;
element_to_binary({xml_stream_start, Name, Attrs}) ->
    <<
      $<,
      Name/binary,
      (attrs_to_list(Attrs))/binary,
      $>
    >>;
element_to_binary({xml_stream_end, Name}) ->
    <<
      $<, $/, Name/binary, $>
    >>;
element_to_binary({xml_cdata, CData}) ->
    crypt(CData);
element_to_binary(#xmlel{name = Name, attrs = Attrs, children = Els}) ->
    case Els of
        [] -> <<
                $<,
                Name/binary,
                (attrs_to_list(Attrs))/binary,
                $/, $>
              >>;
        _  -> Children = << << (element_to_binary(E))/binary >> || E <- Els >>,
              <<
                $<, Name/binary, (attrs_to_list(Attrs))/binary, $>,
                Children/binary,
                $<, $/, Name/binary, $>
              >>
    end.

attrs_to_list(Attrs) ->
    << <<(attr_to_list(A))/binary>> || A <- Attrs >>.

attr_to_list({Name, Value}) ->
    << $\s, Name/binary, $=, $', (crypt(Value))/binary, $' >>.

crypt(S) ->
    << <<(case C of
              $& -> <<"&amp;">>;
              $< -> <<"&lt;">>;
              $> -> <<"&gt;">>;
              $" -> <<"&quot;">>;
              $' -> <<"&apos;">>;
              _ -> <<C>>
          end)/binary>>
       || <<C>> <= S >>.


xml_stream_new(MaxSize) ->
    Port = open_port({spawn_driver, "expat_erl"}, [binary]),
    true = erlang:is_port(Port), %% crash as early as possible.
    #xml_stream_state{port = Port, stack = [], size = 0, maxsize = MaxSize}.


-spec parse(xml_stream_state(), binary()) -> {ok, xml_stream_state(), Events::[xml_stream_el()], term()}.
parse(#xml_stream_state{
         port = Port,
         stack = Stack,
         size = Size,
         maxsize = MaxSize} = State,
      Str) ->
    StrSize = byte_size(Str),
    Res = port_control(Port, ?PARSE_COMMAND, Str),
    { NewStack, NewSize, Events } =
        lists:foldl(fun parse_data/2,
                    {Stack, Size + StrSize, []},
                    binary_to_term(Res)),
    NewSize > MaxSize andalso
        erlang:error( {xml_stream_error, <<"XML stanza is too big">>} ),
    {
      ok,
      State#xml_stream_state{ stack = NewStack,
                              size = NewSize},
      lists:reverse(Events)
    }.

parse_data(Data, { Stack, StrSize, Events } ) ->
    { NewSt, NewEvents } = process_data(Data,{Stack,Events}),
    case NewSt of
        [_] -> {NewSt, 0, NewEvents};
        _ -> {NewSt, StrSize, NewEvents}
    end.


process_data(Data, {Stack,Events}) ->
    case Data of
        {?XML_START, {Name, Attrs}} ->
            process_data_xml_start(Name, Attrs, {Stack,Events});
        {?XML_END, Name} ->
            process_data_xml_end(Name, {Stack,Events});
        {?XML_CDATA, CData} ->
            process_data_xml_cdata(CData, {Stack,Events});
        {?XML_ERROR, Err} ->
            process_data_xml_error(Err,{Stack,Events})
    end.

process_data_xml_start(Name, Attrs, {Stack,Events}) ->
    if
        Stack == [] ->
            NewEvents = [ {xml_stream_start, Name, Attrs} | Events ],
            %% There is no need to store name or attributes of
            %% stream opening element as it is not used
            %% anymore.
            NewStack = [xml_stream_start],
            { NewStack, NewEvents };
        true ->
            NewStack = [ #xmlel{name = Name, attrs = Attrs, children = []}
                         | Stack],
            { NewStack, Events}
    end.

process_data_xml_end(Name, {Stack,Events}) ->
    case Stack of
        [xml_stream_start] ->
            NewEvents = [{xml_stream_end, Name} | Events],
            NewStack = [],
            {NewStack, NewEvents};
        [ #xmlel{name = Name, attrs = Attrs, children = Els},
          xml_stream_start ] ->
            NewEl = #xmlel{name = Name, attrs = Attrs,
                           children = lists:reverse(Els)},
            %% here is strange, c2s.erl only accept mixed type, not consistent.
            %% in order to minimize modification, here send NewEl directly.
            %%    NewEvents = [{xml_stream_element, NewEl} | Events],
            NewEvents = [NewEl | Events],
            NewStack = [xml_stream_start],
            { NewStack, NewEvents };
        [#xmlel{name = Name, attrs = Attrs, children = Els},
         #xmlel{name = Name1, attrs = Attrs1, children = Els1}
         | Tail] ->
            NewEl = #xmlel{name = Name, attrs = Attrs,
                           children = lists:reverse(Els)},
            NewStack = [{xmlel, Name1, Attrs1, [NewEl | Els1]} | Tail],
            { NewStack, Events }
    end.

process_data_xml_cdata(CData, {Stack,Events}) ->
    case Stack of
        [xml_stream_start] ->
            NewEvents = [ {xml_stream_cdata, CData} | Events ],
            NewStack = [xml_stream_start],
            {NewStack, NewEvents};

        %% Merge CDATA nodes if they are contiguous
        %% This does not change the semantic: the split in
        %% several CDATA nodes depends on the TCP/IP packet
        %% fragmentation
        [#xmlel{name = Name, attrs = Attrs,
                children = [{xmlcdata, PreviousCData} | Els]}
         | Tail] ->
            NewStack = [#xmlel{name = Name, attrs = Attrs,
                               children =
                                   [{xmlcdata,
                                     iolist_to_binary([PreviousCData, CData])}
                                    | Els]}
                        | Tail],
            {NewStack, Events};
        %% No previous CDATA
        [#xmlel{name = Name, attrs = Attrs, children = Els}
         | Tail] ->
            NewStack = [#xmlel{name = Name, attrs = Attrs,
                               children = [{xmlcdata, CData} | Els]}
                        | Tail],
            {NewStack, Events};
        [] ->
            NewStack = [],
            {NewStack, Events}
    end.

process_data_xml_error(Err,{_Stack,_Events}) ->
    erlang:error({xml_stream_error, Err}).
