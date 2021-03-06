-module(dummy_sink).
-compile([export_all]).
-include("sp_cmd.hrl").
new(Vars) ->
    ej_vars:add_module(?MODULE,
                       #{
                          name => ?MODULE,
                          output => undefined
                        },
                       Vars
                      ).

dl(#sp_cmd{args = Args}, Vars) ->
    io:format(user,"~p~n",[Args]),
    ej_vars:set(output, Args, ?MODULE, Vars).


get_output(Vars) ->
    ej_vars:get(output, ?MODULE, Vars).
