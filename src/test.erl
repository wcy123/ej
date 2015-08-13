-module(test).
-compile(export_all).
-export([on_init/2,on_data/4]).
on_init(_Pid, Socket) ->
    Response = "0>: Hello\n",
    ok = gen_tcp:send(Socket, Response),
    ok = ej_c2s_main_loop:set_state(Socket, 1).
on_data(_Pid, Socket, Data, State) ->
    Response = [io_lib:format("~p>: ", [State]), Data],
    gen_tcp:send(Socket, Response),
    ok = ej_c2s_main_loop:set_state(Socket, 1 + State).
on_close(_Pid, Socket, State) ->
    error_logger:info_report({debug, ?MODULE, ?LINE, Socket, State}).
crash() ->
    1/0.


test() ->
    et_viewer:start([
        {title,"Coffee Order"},
        {trace_global,true},
        {trace_pattern,{et,max}},
        {max_actors,10}
      ]),
      %% dbg:p(all,call),
      %% dbg:tpl(et, trace_me, 5, []),
      Drink = {drink,iced_chai_latte},
      Size = {size,grande},
      Milk = {milk,whole},
      Flavor = {flavor,vanilla},
      et:trace_me(99,customer,barrista1,place_order,[Drink,Size,Milk,Flavor]),
      et:trace_me(80,barrista1,register,enter_order,[Drink,Size,Flavor]),
      et:trace_me(80,register,barrista1,give_total,"$5"),
      et:trace_me(80,barrista1,barrista1,get_cup,[Drink,Size]),
      et:trace_me(80,barrista1,barrista2,give_cup,[]),
      et:trace_me(90,barrista1,customer,request_money,"$5"),
      et:trace_me(90,customer,barrista1,pay_money,"$5"),
      et:trace_me(80,barrista2,barrista2,get_chai_mix,[]),
      et:trace_me(80,barrista2,barrista2,add_flavor,[Flavor]),
      et:trace_me(80,barrista2,barrista2,add_milk,[Milk]),
      et:trace_me(80,barrista2,barrista2,add_ice,[]),
      et:trace_me(80,barrista2,barrista2,swirl,[]),
      et:trace_me(80,barrista2,customer,give_tasty_beverage,[Drink,Size]),
      ok.
