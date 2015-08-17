%%%-------------------------------------------------------------------
%%% @author chunywan <wcy123@gmail.com>
%%% @copyright (C) 2015, chunywan
%%% @doc
%%%
%%% @end
%%% Created :  4 Aug 2015 by chunywan <wcy123@gmail.com>
%%%-------------------------------------------------------------------
-module(ej_tcp_stub).



%% API

%% callbacks for upper layer
%% ej_c2s_main_loop callbacks
-export([
         new/1,
         ul/2,
         dl/2,
         terminate/2
        ]).
new(Vars) ->
    ej_vars:add_module(?MODULE,
                       #{
                          socket => undefined
                        },
                       Vars).
ul({tcp, Socket, Data}, Vars) ->
    NewVars = ej_vars:set(socket, Socket, ?MODULE, Vars),
    ej_c2s:ul({data, Data}, ?MODULE, NewVars).
dl({data, Data}, Vars) ->
    io:write(Data),
    %% [{Data, Vars}]),
    Vars;
dl( _Args, Vars) ->
    Vars.
terminate(_Socket, Vars) ->
    Vars.
