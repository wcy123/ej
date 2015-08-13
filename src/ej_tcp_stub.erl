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
         init/0,
         ul/2,
         dl/2,
         terminate/2
        ]).
init() ->
    #{}.
ul({tcp, _Socket, Data}, Vars) ->
    ej_c2s_vars:ul({data, Data}, ?MODULE, Vars).
dl(_Args, Vars) ->
    Vars.

terminate(_Socket, Vars) ->
    Vars.
