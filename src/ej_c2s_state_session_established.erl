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

-spec new(Vars :: ej_vars:ej_vars()) -> ej_vars:ej_vars().
new(Vars) ->
    ej_vars:add_module(?MODULE, #{
                         }, Vars).

ul(#sp_cmd{ args = {xml_stream_element, El} }, Vars) ->
    Vars.
