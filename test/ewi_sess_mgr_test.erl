-module(ewi_sess_mgr_test).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    {ok, Meck} = start_meck(),
    try
        {ok, Pid} = ewi_sess_mgr:start_link(),
        ok = ewi_sess_mgr:stop(),
        ok = test_utils:wait_for_process_to_die(Pid, 100)
    after
        stop_meck(Meck)
    end,
    ok.


session_timeout_test() ->
    {ok, Meck} = start_meck(),
    application:set_env(ewi_sess, timeout, 400),
    {ok, Pid} = ewi_sess_mgr:start_link(),
    try

        {ok, Session} = ewi_sess_mgr:new_session(),
        true = is_map(Session),
        true = ewi_sess_mgr:does_session_exist(Session),
        timer:sleep(500),
        false = ewi_sess_mgr:does_session_exist(Session)

    after
        ok = ewi_sess_mgr:stop(),
        ok = test_utils:wait_for_process_to_die(Pid, 100),
        stop_meck(Meck)
    end,
    ok.

session_test() ->
    {ok, Meck} = start_meck(),
    application:set_env(ewi_sess, timeout, 400),
    {ok, Pid} = ewi_sess_mgr:start_link(),
    try
        {error, not_found} = ewi_sess_mgr:lookup_session(unknown),
        {ok, Session} = ewi_sess_mgr:new_session(<<"DATA">>),
        Token = maps:get(token, Session),
        {ok, Session} = ewi_sess_mgr:lookup_session(Token),
        <<"DATA">> = maps:get(data, Session),
        SessPid = maps:get(pid, Session),
        ok = ewi_sess_session:close(SessPid),
        timer:sleep(200),
        false = ewi_sess_mgr:does_session_exist(Session),
        {error, not_found} = ewi_sess_mgr:lookup_session(Token)

    after
        ok = ewi_sess_mgr:stop(),
        ok = test_utils:wait_for_process_to_die(Pid, 100),
        stop_meck(Meck)
    end,
    ok.

close_all_test() ->
    {ok, Meck} = start_meck(),
    application:set_env(ewi_sess, timeout, 400),
    {ok, Pid} = ewi_sess_mgr:start_link(),
    try
        {ok, S1} = ewi_sess_mgr:new_session(),
        {ok, S2} = ewi_sess_mgr:new_session(),
        {ok, S3} = ewi_sess_mgr:new_session(),
        {ok, S4} = ewi_sess_mgr:new_session(),
        true = ewi_sess_mgr:does_session_exist(S1),
        true = ewi_sess_mgr:does_session_exist(S2),
        true = ewi_sess_mgr:does_session_exist(S3),
        true = ewi_sess_mgr:does_session_exist(S4),

        ok = ewi_sess_mgr:close_all_sessions(),
        false = ewi_sess_mgr:does_session_exist(S1),
        false = ewi_sess_mgr:does_session_exist(S2),
        false = ewi_sess_mgr:does_session_exist(S3),
        false = ewi_sess_mgr:does_session_exist(S4),
        ok

    after
        ok = ewi_sess_mgr:stop(),
        ok = test_utils:wait_for_process_to_die(Pid, 100),
        stop_meck(Meck)
    end,
    ok.

garbage_test() ->
    {ok, Meck} = start_meck(),
    try
        {ok, Pid} = ewi_sess_mgr:start_link(),

        gen_server:call(Pid, garbage),
        gen_server:cast(Pid, garbage),
        Pid ! garbage,

        ok = ewi_sess_mgr:stop(),
        ok = test_utils:wait_for_process_to_die(Pid, 100)
    after
        stop_meck(Meck)
    end,
    ok.

start_meck() ->
    MeckModules = [ewi_sess_session_sup],

    NewSession =  fun(Config) ->
                          ewi_sess_session:start_link(Config)
                      end,
    ok = meck:expect(ewi_sess_session_sup, new_session, NewSession),
    {ok, MeckModules}.


stop_meck(MeckModules) ->
    ok = test_utils:meck_done(MeckModules).
