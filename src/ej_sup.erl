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
                  5222,
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
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 1},
    {ok,
     { SupFlags,
       [Hook,
        Random,
        PortGC,
        EventManager,
        C2SMainLoop,
        C2SListener]
     }}.
