-module(ej_port_gc).
-export([start_link/0, stop/1]).

%% internal export
-export([main/0, loop/0]).
-spec start_link() -> {ok, pid()}.
start_link() ->
    ej_utils:maybe_start_link(?MODULE,?MODULE, main,[]).

-spec stop(pid()) -> boolean().
stop(Pid) ->
    Pid ! stop,
    timer:sleep(500),
    not(is_process_alive(Pid)).

-spec main() -> none().
main()->
    process_flag(trap_exit,true),
    ?MODULE:loop().

-spec loop() -> no_return().
loop() ->
    receive
        {'EXIT', Port, _Reason}
          when is_port(Port)
               ->
            catch erlang:port_close(Port),
            loop();
        stop ->
            ok; %% exit the loop
        {'EXIT', From, Reason} ->
            erlang:error({what,{From,Reason}});
        E ->
            erlang:error(E)
    end.
