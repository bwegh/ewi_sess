-module(ewi_sess_mgr).

-behaviour(gen_server).
-export([
         lookup_session/1,
         new_session/0,
         new_session/1,
         close_all_sessions/0,

         session_closing/0,

         start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).


-record(state,{ ets_uuid = undefined,
                ets_pid = undefined
              }).
-define(ETS, ewi_sess_token).
lookup_session(Token) ->
    Result = ets:lookup(?ETS, Token),
    return_lookup_result(Result).

new_session() ->
    new_session(false, undefined).

new_session(Data) ->
    new_session(true, Data).

close_all_sessions() ->
    gen_server:call(?MODULE, close_all_sessions).


session_closing() ->
    gen_server:call(?MODULE, session_closing).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, noparams, []).

init(noparams) ->
    ewi_sess_token = ets:new(ewi_sess_token, [set, protected, named_table]),
    EtsUuid = ets:new(ewi_sess_id, [set, private]),
    EtsPid = ets:new(ewi_sess_pid, [set, private]),
    {ok, #state{ets_uuid = EtsUuid, ets_pid = EtsPid}}.

handle_call(new_session, _From, State) ->
    {ok, Config} = gen_id_token(State),
    {ok, Pid} = ewi_sess_session_sup:new_session(Config),
    ok = update_session(Pid, Config, State),
    {reply, {ok, Pid}, State};
handle_call(session_closing, {Pid, _}, State) ->
    ok = delete_session(Pid, State),
    {reply, ok, State};
handle_call(close_all_sessions, _From,
            #state{ets_uuid = EtsUuid, ets_pid = EtsPid} = State) ->
    true = ets:delete_all_objects(?ETS),
    true = ets:delete_all_objects(EtsUuid),
    Delete = fun({Pid, _, _}, _) ->
                     ok = ewi_sess_session:shutdown(Pid),
                     ok
             end,
    ok = ets:foldl(Delete, ok, EtsPid),
    true = ets:delete_all_objects(EtsPid),
    {reply, ok, State};
handle_call(_,  _, State) ->
    {ok, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


new_session(WithData, Data) ->
    {ok, Pid} = gen_server:call(?MODULE, new_session),
    ok = maybe_add_data(WithData, Data, Pid),
    ewi_sess_session:get_session(Pid).

maybe_add_data(true, Data, Pid) ->
    ok = ewi_sess_session:set_data(Data, Pid);
maybe_add_data(false, _, _) ->
    ok.


gen_id_token(#state{ets_uuid = EtsUuid} = State) ->
    Length = application:get_env(ewi_sess, token_length, 128),
    Uuid = uuid:uuid_to_string(uuid:get_v4()),
    Token = base64url:encode(crypto:strong_rand_bytes(Length)),
    UuidRes = ets:lookup(EtsUuid, Uuid),
    TokenResult = ets:lookup(?ETS, Token),
    DoStore = (UuidRes == []) and (TokenResult == []),
    maybe_store(DoStore, Uuid, Token, State).

maybe_store(true, Uuid, Token, #state{ets_uuid = EtsUuid}) ->
    true = ets:insert_new(EtsUuid, {Uuid, undefined}),
    true = ets:insert_new(?ETS, {Token, undefined}),
    {ok, #{uuid => Uuid, token => Token}};
maybe_store(false, _, _, State) ->
    gen_id_token(State).


update_session(Pid, #{uuid := Uuid, token := Token},
               #state{ets_uuid = EtsUuid, ets_pid=EtsPid}) ->
    true = ets:insert_new(EtsPid, {Pid, Uuid, Token}),
    true = ets:insert(EtsUuid, {Uuid, Pid}),
    true = ets:insert(?ETS, {Token, Pid}),
    ok.

delete_session(Pid, #state{ets_pid = EtsPid} = State) ->
    Result = ets:lookup(EtsPid, Pid),
    true = ets:delete(EtsPid, Pid),
    maybe_delete_entries(Result, State).

maybe_delete_entries([{_, Uuid, Token}], #state{ets_uuid = EtsUuid}) ->
    true = ets:delete(EtsUuid, Uuid),
    true = ets:delete(?ETS, Token),
    ok;
maybe_delete_entries(_, _) ->
    ok.



return_lookup_result([]) ->
    {error, not_found};
return_lookup_result([{_, Pid}]) ->
    ewi_sess_session:get_session(Pid);
return_lookup_result(_) ->
    {error, internal}.
