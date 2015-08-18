my_test_case_0() ->
    [].
my_test_case_0(_Config) ->
    _Vars = ej_c2s:new(),
    ok.

init_per_testcase_my_test_case_1(Config) ->
    process_flag(trap_exit,true),
    {ok, CollectorPid } =
        et_collector:start_link([{trace_global, true},
                                 {trace_port, 4477},
                                 {trace_pattern, {et,max}}
                                ]),
    %%{ok, _Pid1} = et_collector:start_trace_port(4477),
    %%{trace_client_pid, _Pid2} = et_collector:start_trace_client(CollectorPid, ip, 4477),
    %%{old_pattern, _ }  = et_collector:change_pattern(CollectorPid, {et,max}),
    [{ collector_pid, CollectorPid} | Config ].
end_per_testcase_my_test_case_1(Config) ->
    CollectorPid = proplists:get_value(collector_pid, Config),
    et_collector:iterate(CollectorPid, first, infinity, fun replay_event/2, []),
    catch et_collector:stop(CollectorPid),
    Config.



my_test_case_1(_Config) ->
    Vars0 = ej_c2s:new(),
    true = is_map(Vars0),
    Vars1 = ej_c2s:ul({tcp, 1,  << "<?xml version='1.0'?>" >>}, Vars0),
    true = is_map(Vars1),
    Data = << "<stream:stream to='localhost' xmlns:stream='http://etherx.jabber.org/streamsx' xmlns='jabber:client' version='1.0'>" >>,
    Vars2 = ej_c2s:ul({tcp, 1,  Data}, Vars1),
    true = is_map(Vars2),
    Res = {
      {me, self()}
      %%Vars2,
    },

    Res.

replay_event(#event{
                detail_level = DetailLevel,
                trace_ts = TraceTs,
                event_ts = _EventTs,
                from = From,
                to = To,
                label = Label,
                contents = Contents
               }, _ ) ->
    {{Year,Month, Day}, {Hour, Minute, Second} } = calendar:now_to_local_time(TraceTs),
    ct:log
        %%io:format
      ("~p:~p:~p ~p:~p:~p ~p |~p| -----> ~p -----> ~p ~n                    ~p~n"
          ,[Year, Month, Day, Hour, Minute, Second,
            DetailLevel, From,Label, To,
            Contents]).
