-module(ewi_sess).

%% API exports
-export([
         start/0,
         new_session/0
         %% get_session/1,
         %% close_session/1,
         %% close_all_sessions/0
        ]).

%%====================================================================
%% API functions
%%====================================================================

start() ->
    create_table_if_needed().

new_session() ->
    ok.



%%====================================================================
%% Internal functions
%%====================================================================
-define(TABLE, ewi_sess_table).

create_table_if_needed() ->
    create_table_if_undefined(ets:info(?TABLE)).

create_table_if_undefined(undefined) ->
    ?TABLE = ets:new(?TABLE, [named_table]),
    ok;
create_table_if_undefined(_) ->
    ok.
