%% @doc
%% this module contains the global variables for a c2s object.
-module(ej_c2s).
-include("sp_cmd.hrl").
-author('wcy123@gmail.com').
-export([
         new/0,
         ul/3,
         dl/3,
         %% add_top_module/2,
         add_bottom_module/2
        ]).
-spec new() -> ej_vars:ej_vars().
new() ->
    InitVars = #{
      ej_tcp_stub => #{
        detail_level => 30,
        name => ej_xml_stream,
        ul_entity => ej_xml_stream,
        dl_entity => undefined
       },
      ej_xml_stream => #{
        detail_level => 20,
        name => ej_xml_stream,
        ul_entity => ej_c2s_state,
        dl_entity => ej_tcp_stub
       },
      ej_c2s_state => #{
        name => ej_c2s_state,
        detail_level => 10,
        ul_entity => undefined,
        dl_entity => ej_xml_stream
       }
     },
    ej_vars:new([ej_c2s_state,ej_xml_stream,ej_tcp_stub], InitVars).
-spec dl( Cmd :: #sp_cmd{},
          UpperModule:: ej_vars:ej_module(),
          Vars :: ej_vars:ej_vars()) -> ej_vars:ej_vars().
dl(Cmd, UpperModule, Vars) ->
    LowerModule = ej_vars:get(dl_entity, UpperModule, Vars),
    DetailLevel = get_detail_level(LowerModule,UpperModule,Vars),
    et:trace_me(DetailLevel,UpperModule, LowerModule,
                Cmd#sp_cmd.label, [{args, Cmd#sp_cmd.args}]),
    LowerModule:dl(Cmd, Vars).

-spec ul( Cmd :: #sp_cmd{},
          LowerModule :: ej_vars:ej_module(),
          Vars :: ej_vars:ej_vars()) -> ej_vars:ej_vars().
ul(Cmd, LowerModule, Vars) ->
    UpperModule = ej_vars:get(ul_entity, LowerModule,Vars),
    DetailLevel = get_detail_level(LowerModule,UpperModule,Vars),
    et:trace_me(DetailLevel,LowerModule, UpperModule,
                Cmd#sp_cmd.label, [{args, Cmd#sp_cmd.args}]),
    UpperModule:ul(Cmd,Vars).

%% add_top_module(Module, Vars) ->
%%     CurrentState = ej_vars:get(ul_entity, ej_c2s_state, Vars),
%%     NewVars = ej_vars:new([Module],
%%                           maps:puts(Module, #{
%%                                       detail_level => 5,
%%                                       dl_entity => CurrentState
%%                                      }, Vars)),
%%     ej_vars:set(ul_entity,Module, CurrentState,NewVars).
-spec add_bottom_module( Module :: ej_vars:ej_module(),
                         Vars :: ej_vars:ej_vars()) -> ej_vars:ej_vars().
add_bottom_module(Module, Vars) ->
    NewVars = ej_vars:new([Module],
                          maps:put(Module, #{
                                     detail_level => 5,
                                     ul_entity => ej_tcp_stub
                                    }, Vars)),
    ej_vars:set(dl_entity, Module,ej_tcp_stub, NewVars).


-spec get_detail_level(ej_vars:ej_module(),
                       ej_vars:ej_module(),
                       ej_vars:ej_vars()) -> integer().
get_detail_level(M1,M2,Vars) ->
    (ej_vars:get(detail_level, M1 ,Vars) + ej_vars:get(detail_level, M2,Vars))
        div 2.
