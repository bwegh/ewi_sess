-module(ewi_sess_session).
-behaviour(gen_server).
-export([
         set_data/2,
         get_data/1,
         get_session/1,
         close/1,

         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).


get_data(Session) ->
    gen_server:call(to_pid(Session), get_data).

set_data(Data, Session) ->
    gen_server:call(to_pid(Session), {set_data, Data}).

get_session(Session) ->
    gen_server:call(to_pid(Session),get_session).


close(Session) ->
    gen_server:call(to_pid(Session), close).


-record(state,{ id = undefined,
                token = undefined,
                data = undefined,
                closing = false
              }).


init(#{id := Id, token := Token}) ->
    {ok, #state{id = Id, token = Token}}.

handle_call(_Msg, _From, #state{closing = true}) ->
    {error, closing};
handle_call(get_session, _From, State) ->
    {reply, {ok, to_session(State)}, State};
handle_call(get_data, _From, #state{data = Data} = State) ->
    {reply, {ok, Data}, State};
handle_call({set_data, Data}, _From, State) ->
    {reply, ok, State#state{data = Data}};
handle_call(close, _From, State) ->
    %% TODO: implement
    {reply, ok, State#state{closing = true}};
handle_call(_Msg, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

to_pid(Pid) when is_pid(Pid) ->
    Pid;
to_pid(#{pid := Pid}) ->
    Pid.

to_session(#state{id = Id, token = Token, data = Data}) ->
    #{id => Id, token => Token, data => Data}.
