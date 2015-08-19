-module(ej_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,start_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
start_child(ChildSpec) ->
    supervisor:start_child(?MODULE, ChildSpec).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Hook = #{
      id => ejabberd_hooks,
      start => {ejabberd_hooks, start_link, []},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker,
      modules => [ejabberd_hooks]},
    Random = #{
      id => randoms,
      start => { randoms, start, [] },
      restart => permanent,
      shutdown => 1000,
      type => worker,
      modules => [ randoms ]
     },
    Random = #{
      id => randoms,
      start => { randoms, start, [] },
      restart => permanent,
      shutdown => 1000,
      type => worker,
      modules => [ randoms ]
     },


    PortGC = #{
      id => ej_port_gc,
      start => { ej_port_gc, start_link, [] },
      restart => permanent,
      shutdown => 1000,
      type => worker,
      modules => [ ej_port_gc ]
    },
    EventManager = #{
      id => ej_event_manager,
      start => { ej_event_manager, start_link, [] },
      restart => permanent,
      shutdown => 1000,
      type => worker,
      modules => [ ej_event_manager ]
     },
    C2SMainLoop = #{
       id => ej_c2s_main_loop,
       start => { ej_c2s_main_loop, start_link, [ej_c2s_raw_transport] },
       restart => permanent,
       shutdown => 1000,
       type => worker,
       modules => [ ej_c2s_sup ]
     },
    C2SListener = #{
      id => ej_listener_c2s_sup,
      start => { etcp_listener_sup, start_link,
                 [ej_listener_c2s_sup,
                  5422,
                  [{reuseaddr,true},
                   {active,false},
                   {packet, 0},
                   binary
                  ],fun ej_c2s_main_loop:get_pid/0] },
      restart => permanent,
      shutdown => 1000,
      type => supervisor,
      modules => [ etcp_listener_sup ]
     },
    SM = #{
      id => ejabberd_sm,
      start => { ejabberd_sm, start_link, [] },
      restart => permanent,
      shutdown => 1000,
      type => worker,
      modules => [ ejabberd_sm ]
     },
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 1},
    {ok,
     r18_to_r17({ SupFlags,
                  [
                   Hook,
                   Random,
                   PortGC,
                   EventManager,
                   C2SMainLoop,
                   C2SListener,
                   SM
                  ]
                })}.

%% R17 does not support map pattern matching.
%% r18_to_r17(#{
%%               id => Id,
%%               start => Start,
%%               restart => Restart,
%%               shutdown => Shutdown,
%%               type => Type,
%%               modules => Modules
%%             } = _ChildSpec ) ->
%%     {Id, Start, Restart, Shutdown, Type, Modules};
%% r18_to_r17(#{
%%               strategy => Strategy,
%%               intensity => Intensity,
%%               period => Period}) ->
%%     {Strategy, Intensity, Period};
%% r18_to_r17({SupFlags, ChildrenSpec}) ->
%%     { r18_to_r17(SupFlags),
%%       lists:maps(fun r18_to_r17/1, ChildrenSpec) }.

r18_to_r17_child_spec(ChildSpec ) ->
    {
      maps:get(id,ChildSpec),
      maps:get(start,ChildSpec),
      maps:get(restart,ChildSpec),
      maps:get(shutdown,ChildSpec),
      maps:get(type,ChildSpec),
      maps:get(modules,ChildSpec)
    }.
r18_to_r17_sup_flag(SupFlags) ->
    {
      maps:get(strategy,SupFlags),
      maps:get(intensity,SupFlags),
      maps:get(period,SupFlags)
    }.
r18_to_r17({SupFlags, ChildrenSpec}) ->
    { r18_to_r17_sup_flag(SupFlags),
      lists:map(fun r18_to_r17_child_spec/1, ChildrenSpec) }.
