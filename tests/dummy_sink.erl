-module(dummy_sink).
-compile([export_all]).

new(Vars) ->
    ej_vars:add_module(?MODULE,
                       #{
                          name => ?MODULE
                        },
                       Vars
                      ).

dl(Args, Vars) ->
    io:format("hahaf ~s~n", [ej_utils:term_to_string(Args)]),
    Vars.
