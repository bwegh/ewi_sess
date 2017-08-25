-module(ewi_sess_mgr).

-behaviour(gen_server).
-export([

         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).


-record(state,{ ets = undefined}).


init(noparams) ->
    ets:new(ewi_sess_token, [set, protected, named_table]),
    Ets = ets:new(ewi_sess_id, [set, private]),
    {ok, #state{ets = Ets}}.

handle_call(_Msg, _From, State) ->
    {ok, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
