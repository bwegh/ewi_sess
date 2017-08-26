-module(ewi_sess).

%% API exports
-export([
         start_link/0,
         new_session/0,
         new_session/1,
         get_session/1,
         close_session/1,
         close_all_sessions/0
        ]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    application:ensure_all_started(?MODULE),
    supervisor:start_link(ewi_sess_sup, []).

new_session() ->
    ewi_sess_mgr:new_session().

new_session(Data) ->
    ewi_sess_mgr:new_session(Data).

get_session(Token) ->
    ewi_sess_mgr:lookup_session(Token).

close_session(Session) ->
    ewi_sess_session:close(Session).

close_all_sessions() ->
    ewi_sess_mgr:close_all_sessions().
