%% @doc
%% this module contains the global variables for a c2s object.
-module(ej_c2s).
-author('wcy123@gmail.com').
-export([
         new/0,
         ul/2,
         ul/3,
         dl/2,
         dl/3
        ]).
-spec new() -> ej_vars:ej_vars().
new() ->
    InitVars = #{
      ej_c2s_state => #{
        name => ej_c2s_state,
        detail_level => 10,
        ul_entity => undefined,
        dl_entity => ej_xml_stream
       },
      ej_xml_stream => #{
        detail_level => 20,
        name => ej_xml_stream,
        ul_entity => ej_c2s_state,
        dl_entity => ej_tcp_stub
       },
      ej_tcp_stub => #{
        detail_level => 30,
        name => ej_xml_stream,
        ul_entity => ej_xml_stream,
        dl_entity => undefined
       },
      top => ej_c2s_state,
      bottom => ej_tcp_stub
     },
    ej_vars:new([ej_c2s_state,ej_xml_stream,ej_tcp_stub], InitVars).

dl(Args, Vars) ->
    Module = maps:get(top, Vars),
    DetailLevel = ej_vars:get(detail_level, Module, Vars),
    et:trace_me(DetailLevel, top, Module,
                erlang:element(1,Args), [{args, Args}]),
    %% die as early as possible, perserve stack info
    Module:dl(Args, Vars).


dl(Args, UpperModule, Vars) ->
    LowerModule = ej_vars:get(dl_entity, UpperModule, Vars),
    DetailLevel = get_detail_level(LowerModule,UpperModule,Vars),
    et:trace_me(DetailLevel,UpperModule, LowerModule,
                erlang:element(1,Args), [{args, Args}]),
    LowerModule:dl(Args, Vars).


ul(Args, Vars) ->
    Module = maps:get(bottom, Vars),
    DetailLevel = ej_vars:get(detail_level, Module ,Vars),
    et:trace_me(DetailLevel,bottom, Module,
                erlang:element(1,Args), [{args, Args}]),
    Module:ul(Args, Vars).

ul(Args, LowerModule, Vars) ->
    UpperModule = ej_vars:get(ul_entity, LowerModule,Vars),
    DetailLevel = get_detail_level(LowerModule,UpperModule,Vars),
    et:trace_me(DetailLevel,LowerModule, UpperModule,
                erlang:element(1,Args), [{args, Args}]),
    UpperModule:ul(Args,Vars).


get_detail_level(M1,M2,Vars) ->
    (ej_vars:get(detail_level, M1 ,Vars) + ej_vars:get(detail_level, M2,Vars))
        div 2.
