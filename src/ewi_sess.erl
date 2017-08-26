-module(ewi_sess).

%% API exports
-export([
         start_link/0,
         new_session/0,
         new_session/1,
         get_session/1,
         update_session/2,
         does_session_exist/1,
         close_session/1,
         close_all_sessions/0,

         create_cookie/2,
         cookie_session/2
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

update_session(Data, Session) ->
    ewi_sess_session:set_data(Data, Session).

does_session_exist(Token) ->
    ewi_sess_mgr:does_session_exist(Token).

close_session(Session) ->
    ewi_sess_session:close(Session).

close_all_sessions() ->
    ewi_sess_mgr:close_all_sessions().

create_cookie(CookieName, #{token := Token, timeout := Timeout}) ->
   create_cookie(CookieName, Token, Timeout).

cookie_session(CookieName, App) ->
    fun(Env) ->
            cookie_session_app(CookieName, App, Env)
    end.


%%====================================================================
%% internal functions
%%====================================================================



cookie_session_app(CookieName, App, Env0) ->
    {CookieToken, Env} = cookie_data(CookieName, Env0),
    Session = try_find_session(CookieToken),
    NewEnv = update_env(Session, Env),
    handle_session_app(psycho:call_app(App, NewEnv), CookieName, Session).

handle_session_app({{Code, _} = Status, Headers, Body}, CookieName, Session) ->
    Exists = does_session_exist(Session),
    SuccessCode = (Code >= 200) and (Code < 400),
    CookieAction = cookie_action(Exists, SuccessCode),
    NewHeaders = maybe_set_cookie(CookieAction, CookieName, Session, Headers),
    {Status, NewHeaders, Body}.

cookie_action(true, true) ->
    set;
cookie_action(false, true) ->
    clear;
cookie_action(_, _) ->
    none.

maybe_set_cookie(set, Name, #{timeout := Timeout, toke := Token}, Headers) ->
    CookieHeader = create_cookie(Name, Token, Timeout),
    [ CookieHeader | Headers ];
maybe_set_cookie(clear, Name, _, Headers) ->
    CookieHeader = create_cookie(Name, deleted, 0),
    [ CookieHeader | Headers ];
maybe_set_cookie(_, _, _, Headers) ->
    Headers.

update_env(undefined, Env) ->
    Env;
update_env(Session, Env) ->
    [{ewi_sess, Session} | Env].

cookie_data(Name, Env0) ->
    {Cookies, Env} = psycho_util:ensure_parsed_cookie(Env0),
    {cookie_val(Name, Cookies), Env}.

cookie_val(Name, Cookies) ->
    proplists:get_value(Name, Cookies).

try_find_session(undefined) ->
    undefined;
try_find_session(SessionToken) when is_binary(SessionToken) ->
    handle_session_result(get_session(SessionToken));
try_find_session(SessionToken) when is_list(SessionToken) ->
    try_find_session(list_to_binary(SessionToken)).

handle_session_result({ok, Session}) ->
    Session;
handle_session_result(_) ->
    undefined.


create_cookie(Name, Value, MaxAge) when is_list(Value); is_binary(Value) ->
    Opts = cookie_opts(MaxAge),
    psycho_util:cookie_header(Name, Value, Opts);
    %% case application:get_env(acc_srv,ssl) of
    %%     true ->
    %%         [{secure, true} | BasicOpts];
    %%     _ ->
    %%         BasicOpts
    %% end.
create_cookie(Name, _Value, _MaxAge) ->
    Opts = cookie_opts(0),
    psycho_util:cookie_header(Name, "deleted", Opts).

cookie_opts(MaxAge) ->
    [ {http_only, true}, {max_age, MaxAge}, {path, <<"/">>}].
