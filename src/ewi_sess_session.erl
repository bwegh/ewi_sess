-module(ewi_sess_session).
-behaviour(gen_server).
-export([
         set_data/2,
         get_data/1,
         get_session/1,
         close/1,

         shutdown/1,

         start_link/1,
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

shutdown(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, shutdown).

-record(state,{ uuid = undefined,
                token = undefined,
                data = undefined,
                closing = false,
                timeout = infinity
              }).

start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

init(#{uuid := Id, token := Token}) ->
    Timeout = application:get_env(ewi_sess, timeout, 15 * 60 * 1000),
    {ok, #state{uuid = Id, token = Token, timeout = Timeout}}.

handle_call(_Msg, _From, #state{closing = true, timeout = Timeout} = State) ->
    {reply, {error, closing}, State, Timeout};
handle_call(get_session, _From, #state{timeout = Timeout} = State) ->
    {reply, {ok, to_session(State)}, State, Timeout};
handle_call(get_data, _From, #state{data = Data, timeout = Timeout} = State) ->
    {reply, {ok, Data}, State, Timeout};
handle_call({set_data, Data}, _From, #state{timeout = Timeout} = State) ->
    {reply, ok, State#state{data = Data}, Timeout};
handle_call(close, _From, #state{timeout = Timeout} = State) ->
    {reply, ok, close_session(State), Timeout};
handle_call(_Msg, _From, #state{timeout = Timeout} = State) ->
    {reply, ignored, State, Timeout}.

handle_cast(shutdown, State) ->
    {stop, normal, State};
handle_cast(_Msg, #state{timeout = Timeout} = State) ->
    {noreply, State, Timeout}.

handle_info(timeout, State) ->
    {noreply, close_session(State), 2000};
handle_info(_Msg, #state{timeout = Timeout} = State) ->
    {noreply, State, Timeout}.

close_session(State) ->
    ok = ewi_sess_mgr:session_closing(),
    shutdown(self()),
    State#state{closing = true}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

to_pid(Pid) when is_pid(Pid) ->
    Pid;
to_pid(#{pid := Pid}) ->
    Pid.

to_session(#state{uuid = Id, token = Token, data = Data, timeout = Timeout}) ->
    #{pid => self(), uuid => Id, token => Token, data => Data,
      timeout => Timeout}.
