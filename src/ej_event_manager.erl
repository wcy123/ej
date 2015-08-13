%%%-------------------------------------------------------------------
%%% @author chunywan <wcy123@gmail.com>
%%% @copyright (C) 2015, chunywan
%%% @doc
%%%
%%% @end
%%% Created :  7 Aug 2015 by chunywan <wcy123@gmail.com>
%%%-------------------------------------------------------------------
-module(ej_event_manager).
%% API
-export([start_link/0, add_handler/2, delete_handler/2, notify/1, which_handlers/0]).

%% CALLBACK Functions
-export([init/1]).
%%
-define(SERVER, ?MODULE).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    {ok , Pid } = gen_event:start_link({local, ?SERVER}),
    ej_eh_monitor:add_handler(),
    {ok,Pid}.


%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @end
%%--------------------------------------------------------------------
-spec add_handler(gen_event:handler(),term()) -> term().
add_handler(H,Args) ->
    Children = gen_event:which_handlers(?SERVER),
    case proplists:get_value(H,Children) of
        undefined ->
            gen_event:add_handler(?SERVER, {H,erlang:make_ref()}, Args);
        _ ->
            ok %% already added
    end.
-spec delete_handler(gen_event:handler(),term()) -> term().
delete_handler(H,Args) ->
    Children = gen_event:which_handlers(?SERVER),
    case proplists:get_value(H,Children) of
        undefined ->
            ok; % not added yet.
        Ref ->
            gen_event:delete_handler(?SERVER, {H,Ref}, Args)
    end.
which_handlers() ->
    gen_event:which_handlers(?SERVER).
-spec notify(term()) -> ok.
notify(Event) ->
    gen_event:notify(?SERVER,Event).

%%

init([]) ->
    {ok, {}}.
