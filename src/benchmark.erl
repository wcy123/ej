-module(benchmark).
-author('cooldaemon@gmail.com').

-export([run/1]).

run(Count) ->
    Keys = lists:seq(1, Count),
    lists:foreach(
      fun ({F, TargetName}) ->
              {[SetRunTime, SetWallClock], [GetRunTime, GetWallClock]} = F(Keys),
              io:fwrite(
                "--<~s>--~nset:~p(~p)ms~nget:~p(~p)ms~n",
                [TargetName, SetRunTime, SetWallClock, GetRunTime, GetWallClock]
               )
      end,
      [
       {fun pd/1, "process dictionary"},
       {fun dict/1, "dict"},
       {fun ets/1,  "ets"},
       {fun gb_trees/1, "gb_trees"},
       %{fun array/1, "array"}
       {fun map/1, "map"}
      ]
     ).

pd(Keys) ->
    {_SetResult, SetTimes} = benchmark(
                               fun () -> lists:foreach(fun (N) -> put(N, a) end, Keys) end
                              ),
    {_GetResult, GetTimes} = benchmark(
                               fun () -> lists:map(fun (N) -> get(N) end, Keys) end
                              ),
    {SetTimes, GetTimes}.

dict(Keys) ->
    {Dict, SetTimes} = benchmark(
                         fun () ->
                                 lists:foldl(fun (N, D) -> dict:store(N, a, D) end, dict:new(), Keys)
                         end
                        ),
    {_GetResult, GetTimes} = benchmark(
                               fun () ->
                                       lists:map(fun (N) -> {ok, V} = dict:find(N,Dict), V end, Keys)
                               end
                              ),
    {SetTimes, GetTimes}.

ets(Keys) ->
    Ets = ets:new(test_ets, [bag, private]),
    {_SetResult, SetTimes} = benchmark(
                               fun () ->
                                       lists:foreach(fun (N) -> ets:insert(Ets, {N, a}) end, Keys)
                               end
                              ),
    {_GetResult, GetTimes} = benchmark(
                               fun () ->
                                       lists:map(fun (N) -> [{_K, V}] = ets:lookup(Ets, N), V end, Keys)
                               end
                              ),
    {SetTimes, GetTimes}.

gb_trees(Keys) ->
    {Tree, SetTimes} = benchmark(
                         fun () ->
                                 lists:foldl(
                                   fun (N, T) -> gb_trees:enter(N, a, T) end,
                                   gb_trees:empty(), Keys
                                  )
                         end
                        ),
    BalancedTree = gb_trees:balance(Tree),
    {_GetResult, GetTimes} = benchmark(
                               fun () ->
                                       lists:map(fun (N) -> gb_trees:get(N, BalancedTree) end, Keys)
                               end
                              ),
    {SetTimes, GetTimes}.

array(Keys) ->
    {A, SetTimes} = benchmark(
                               fun () -> lists:foldl(fun (N,A) -> array:set(N, a, A) end, array:new(1000001),Keys) end
                              ),
    {_GetResult, GetTimes} = benchmark(
                               fun () -> lists:map(fun (N) -> array:get(N,A) end, Keys) end
                              ),
    {SetTimes, GetTimes}.

map(Keys) ->
    {M, SetTimes} = benchmark(
                      fun () -> lists:foldl(fun (K,M) -> M#{K => a} end, #{},Keys) end
                     ),
    {_GetResult, GetTimes} = benchmark(
                               fun () -> lists:map(fun (K) -> maps:get(K,M) end, Keys) end
                              ),
    {SetTimes, GetTimes}.

benchmark(TargetFunction) ->
    lists:foreach(fun statistics/1, [runtime, wall_clock]),
    Result = TargetFunction(),
    Times = lists:map(
              fun (Type) -> {_, T} = statistics(Type), T end,
              [runtime, wall_clock]
             ),
    {Result, Times}.

%% Permalink | コメント(0) | トラックバック(0) | 14:55 Process Dictionary、dict、ets、gb_trees の速度比較を含むブックマーク
