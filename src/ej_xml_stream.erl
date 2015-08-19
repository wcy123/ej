%%%-------------------------------------------------------------------
%%% @author chunywan <wcy123@gmail.com>
%%% @copyright (C) 2015, chunywan
%%% @doc
%%%
%%% @end
%%% Created :  4 Aug 2015 by chunywan <wcy123@gmail.com>
%%%-------------------------------------------------------------------
-module(ej_xml_stream).
-define(DETAIL_LEVEL,99).
%% API
%% callbacks for upper layer
%% ej_c2s_main_loop callbacks
-export([
         new/1,
         ul/2,
         dl/2,
         reset_stream/1,
         terminate/2,
         element_to_binary/1
        ]).
-spec new(Vars :: ej_vars:ej_vars()) -> ej_vars:ej_vars().
new(Vars)            ->
    Port = open_port({spawn_driver, "expat_erl"}, [binary]),
    %% now the ej_port_gc owns the port, so that when this processs is
    %% ended, the port still survive and keeps open. whenever a
    %% temporary process aborts, ej_port_gc will be notified.
    true = erlang:port_connect(Port, whereis(ej_port_gc)),
    true = erlang:is_port(Port), %% crash as early as possible.
    ej_vars:add_module(?MODULE,
                       #{
                          port => Port,
                          stack => [],
                          size => 0,
                          maxsize => infinity
                        }, Vars).
ul({data, Data}, Vars) ->
    %% adapter to original code.
    et:trace_me(?DETAIL_LEVEL, ?MODULE, expal, parse , [{data, Data}]),
    {ok, NewVars, Events}  = parse(Data,Vars),
    et:trace_me(?DETAIL_LEVEL, expal, ?MODULE, ok ,
                [{xml, ej_vars:get(?MODULE,NewVars) },
                 {events, Events}]),
    lists:foldl(fun report_event/2, NewVars, Events).


report_event(E, Vars) when is_map(Vars) ->
    ej_c2s:ul(E, ?MODULE, Vars).

dl({send_xml, XMLs}, Vars) ->
    IOData = lists:map(fun element_to_binary/1, XMLs),
    ej_c2s:dl({data, IOData}, ?MODULE, Vars).

reset_stream(Vars) ->
    ej_vars:set(stack, [],?MODULE, Vars).

terminate(_Args, _Vars) ->
    1.



%%%===================================================================
%%% Internal functions
%%%===================================================================
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
element_to_binary({xmlcdata, CData}) ->
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


parse(Str,Vars) ->
    StrSize = byte_size(Str),
    Port = ej_vars:get(port,?MODULE,Vars),
    Stack = ej_vars:get(stack,?MODULE,Vars),
    Size = ej_vars:get(size,?MODULE,Vars),
    MaxSize = ej_vars:get(maxsize,?MODULE,Vars),
    Res = port_control(Port, ?PARSE_COMMAND, Str),
    XmlRawData = binary_to_term(Res),
    { NewStack, NewSize, Events } =
        lists:foldl(fun parse_data/2,
                    {Stack, Size + StrSize, []},
                    XmlRawData),
    NewSize > MaxSize andalso
        erlang:error( {xml_stream_error, <<"XML stanza is too big">>} ),
    Vars0 = ej_vars:set(stack, NewStack,?MODULE, Vars),
    Vars1 = ej_vars:set(size, NewSize, ?MODULE, Vars0),
    {
      ok, Vars1,
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
            NewEvents = [{xml_stream_element,NewEl} | Events],
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
