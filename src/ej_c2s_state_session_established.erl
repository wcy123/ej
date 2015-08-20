-module(ej_c2s_state_session_established).
-include("sp_cmd.hrl").
-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").
-export([
         new/1,
         ul/2
        ]).

new(Vars) ->
    ej_vars:add_module(?MODULE, #{
                         }, Vars).

ul({xml_stream_element, El}, Vars) ->
    Vars.
