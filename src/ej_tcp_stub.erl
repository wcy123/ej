%%%-------------------------------------------------------------------
%%% @author chunywan <wcy123@gmail.com>
%%% @copyright (C) 2015, chunywan
%%% @doc
%%%
%%% @end
%%% Created :  4 Aug 2015 by chunywan <wcy123@gmail.com>
%%%-------------------------------------------------------------------
-module(ej_tcp_stub).
-include("sp_cmd.hrl").


%% API

%% callbacks for upper layer
%% ej_c2s_main_loop callbacks
-export([
         new/1,
         ul/2,
         dl/2,
         %% terminate/2,
         %% set_socket/2
         get_socket/1,
         get_ip/1,
         get_conn_type/1,
         %% @todo add set_ip
         change_shaper/2
        ]).

new(Vars) ->
    ej_vars:add_module(?MODULE,
                       #{
                          socket => undefined,
                          ip => {{127,0,0,1}, {12345} }
                        },
                       Vars).

ul(#sp_cmd{args = {tcp, Socket, _ } } = Cmd, Vars) ->
    NewVars = set_socket(Socket,Vars),
    ej_c2s:ul(Cmd, ?MODULE, NewVars).
dl(#sp_cmd{} = Cmd, Vars) ->
    ej_c2s:dl(Cmd, ?MODULE, Vars).

set_socket(Socket, Vars) ->
    ej_vars:set(socket, Socket, ?MODULE, Vars).
get_socket(Vars) ->
    ej_vars:get(socket, ?MODULE, Vars).

get_ip(Vars) ->
    ej_vars:get(ip, ?MODULE, Vars).


%% todo
change_shaper(_JID,Vars) ->
    Vars.
%% todo
get_conn_type(_Vars) ->
    c2s.
%% get_socket(Vars) ->
%%     ej_vars:get(socket, ?MODULE, Vars).
