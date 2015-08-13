-module(ej_port_gc).
-export([start_link/0]).

%% internal export
-export([main/0]).

start_link() ->
    ej_utils:maybe_start_link(?MODULE,?MODULE, main,[]).


main()->
    process_flag(trap_exit,true),
    proc_lib:init_ack({ok,self()}),
    ?MODULE:loop().

loop() ->
    receive
        {'EXIT', Port, _Reason}
          when is_port(Port)
               -> catch erlang:port_close(Port);
        {'EXIT', From, Reason} ->
            erlang:error({what,{From,Reason}});
        E ->
            erlang:error(E)
    end,
    loop().
