%%%-------------------------------------------------------------------
%%% @author chunywan <wcy123@gmail.com>
%%% @copyright (C) 2015, chunywan
%%% @doc
%%%    this is the main process that manage all sockets.
%%% @end
%%% Created :  8 Aug 2015 by chunywan <wcy123@gmail.com>
%%%-------------------------------------------------------------------
-module(ej_c2s_main_loop).

-behaviour(gen_server).
-include("logger.hrl").
%% API
-export([start_link/1, get_pid/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DETAIL_LEVEL,60).
-record(state, {db = #{}, module}).
%% @private
-export([callback_entry/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%  this function is used as a callback function for etcp_listener.
-spec get_pid() -> {ok, pid()}.
get_pid() ->
    gen_server:call(?SERVER, get_pid).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Module) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Module], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Module]) ->
    {ok, #state{module=Module}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_pid, _From, State) ->
    {reply, {ok, self()}, State};
handle_call({set_state, Socket, ModuleState}, _From, State) ->
    OldTree = State#state.db,
    NewTree = OldTree#{ Socket => ModuleState},
    NewState = State#state{db = NewTree},
    inet:setopts(Socket, [{active,once}]),
    {reply, ok, NewState};
handle_call(get_all_state, _From, State) ->
    {reply, State#state.db, State};
handle_call(_Request, _From, State) ->
    {stop, unknown_api, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({become_controller, Socket}, State) ->
    %% no linking, crash report is error.
    et:trace_me(?DETAIL_LEVEL, main, xml_stream, init, []),
    invoke_call_back(Socket, State#state.module, init, [Socket]),
    {noreply, State};
handle_info({tcp, Socket, Data}, State) ->
    et:trace_me(?DETAIL_LEVEL, tcp, main, tcp_data, [{data, Data}]),
    DB  = State#state.db,
    case maps:get(Socket, DB, undefined) of
        undefined ->
            %% strange, the socket is not found
            ?WARNING_MSG("Socket ~p is not belong me, it is strange~n",[Socket]),
            gen_tcp:close(Socket),
            {noreply, State};
        ModuleState ->
            %% thanks for erlang, the order of evaluation of below
            %% expressions does not matter. In the callback function,
            %% it looks like the ModuleState is already removed.
            et:trace_me(?DETAIL_LEVEL, main, xml_stream, tcp_data, [{data, Data}]),
            invoke_call_back(Socket, State#state.module, handle_info, [{tcp, Socket, Data}, ModuleState]),
            NewDB = maps:remove(Socket, DB),
            NewState = State#state{db = NewDB},
            {noreply, NewState}
    end;
handle_info({tcp_closed, Socket}, State) ->
    et:trace_me(?DETAIL_LEVEL, tcp, main, tcp_closed, []),
    DB  = State#state.db,
    case maps:get(Socket, DB, undefined) of
        none ->
            %% strange, the socket is not found
            ?WARNING_MSG("Socket ~p is not belong me, it is strange~n",[Socket]),
            gen_tcp:close(Socket),
            {noreply, State};
        ModuleState ->
            et:trace_me(?DETAIL_LEVEL, main, xml_stream, tcp_closed, []),
            invoke_call_back(Socket, State#state.module, terminate, [shutdown, ModuleState]),
            NewDB = maps:remove(Socket, DB),
            NewState = State#state{db = NewDB},
            {noreply, NewState}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
invoke_call_back(Socket,M,F,A) ->
    Pid = proc_lib:spawn(?MODULE,callback_entry,[Socket,M,F,A]),
    case gen_tcp:controlling_process(Socket,Pid) of
        ok -> ok;
        {error, closed} -> ok
    end,
    Pid ! become_controller,
    Pid.


callback_entry(Socket,M,F,A) ->
    receive
        become_controller ->
            case erlang:apply(M,F,A) of
                {ok, State} -> set_state(Socket,State);
                _Otherwise -> do_nothing
            end
    after 1000 ->
            erlang:exit(cannot_become_controler)
    end.


set_state(Socket, StateData)->
    Pid = whereis(?SERVER),
    true = is_pid(Pid),
    true = is_process_alive(Pid),
    ok = gen_tcp:controlling_process(Socket, Pid),
    gen_server:call(?SERVER, {set_state,Socket, StateData}).
