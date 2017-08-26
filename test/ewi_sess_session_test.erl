-module(ewi_sess_session_test).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    {ok, Meck} = start_meck(),
    try
        Config = #{uuid => "", token => <<>>},
        {ok, Pid} = ewi_sess_session:start_link(Config),
        ok = ewi_sess_session:close(Pid),
        ok = test_utils:wait_for_process_to_die(Pid, 100)
    after
        stop_meck(Meck)
    end,
    ok.

get_session_test() ->
    {ok, Meck} = start_meck(),
    try
        Config = #{uuid => "", token => <<>>},
        {ok, Pid} = ewi_sess_session:start_link(Config),

        {ok, Session} = ewi_sess_session:get_session(Pid),

        ok = ewi_sess_session:close(Session),
        ok = test_utils:wait_for_process_to_die(Pid, 100)
    after
        stop_meck(Meck)
    end,
    ok.

data_test() ->
    {ok, Meck} = start_meck(),
    try
        Config = #{uuid => "", token => <<>>},
        {ok, Pid} = ewi_sess_session:start_link(Config),

        {ok, undefined} = ewi_sess_session:get_data(Pid),
        ok = ewi_sess_session:set_data(testdata, Pid),
        {ok, testdata} = ewi_sess_session:get_data(Pid),
        {ok, Session} = ewi_sess_session:get_session(Pid),
        testdata = maps:get(data, Session),

        ok = ewi_sess_session:close(Session),
        ok = test_utils:wait_for_process_to_die(Pid, 100)
    after
        stop_meck(Meck)
    end,
    ok.

timeout_test() ->
    {ok, Meck} = start_meck(),
    try
        Config = #{uuid => "", token => <<>>},
        application:set_env(ewi_sess, timeout, 400),
        {ok, Pid} = ewi_sess_session:start_link(Config),
        {ok, undefined} = ewi_sess_session:get_data(Pid),
        ok = ewi_sess_session:set_data(testdata, Pid),
        {ok, testdata} = ewi_sess_session:get_data(Pid),
        {ok, Session} = ewi_sess_session:get_session(Pid),
        testdata = maps:get(data, Session),

        timer:sleep(500),
        ok = test_utils:wait_for_process_to_die(Pid, 10)
    after
        stop_meck(Meck)
    end,
    ok.


garbage_test() ->
    {ok, Meck} = start_meck(),
    try
        Config = #{uuid => "", token => <<>>},
        {ok, Pid} = ewi_sess_session:start_link(Config),

        gen_server:call(Pid, garbage),
        gen_server:cast(Pid, garbage),
        Pid ! garbage,
        {ok, Session} = ewi_sess_session:get_session(Pid),

        ok = ewi_sess_session:close(Session),
        ok = test_utils:wait_for_process_to_die(Pid, 100)
    after
        stop_meck(Meck)
    end,
    ok.

start_meck() ->
    MeckModules = [ewi_sess_mgr],

    SessionClosing =  fun() ->
                              ok
                      end,
    ok = meck:expect(ewi_sess_mgr, session_closing, SessionClosing),
    {ok, MeckModules}.


stop_meck(MeckModules) ->
    ok = test_utils:meck_done(MeckModules).
