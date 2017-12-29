-module(ewi_sess_session).
-behaviour(gen_server).
-export([
         set_data/2,
         get_data/1,
         keep_alive/1,
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

keep_alive(Session) ->
    gen_server:call(to_pid(Session), keep_alive).

close(Session) ->
    gen_server:call(to_pid(Session), close).

shutdown(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, shutdown).

-ifdef(TEST).
-define(TIMEOUT, 100).
-else.
-define(TIMEOUT, 2000).
-endif.

-record(state,{ uuid = undefined,
                token = undefined,
                data = undefined,
                closing = false,
                skip_cookie = false,
                timeout = 0,
                sess_duration = infinity
              }).

start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

init(#{uuid := Id, token := Token}) ->
    SessDuration = application:get_env(ewi_sess, sess_duration, 15 * 60),
    {ok, new_timeout(#state{uuid = Id, token = Token,
                            sess_duration = SessDuration})}.

handle_call(_Msg, _From, #state{closing = true} = State) ->
    {reply, {error, closing}, State, 2000};
handle_call(get_session, _From, State) ->
    {reply, {ok, to_session(State)}, State, ?TIMEOUT};
handle_call(keep_alive, _From, State) ->
    {reply, ok, new_timeout(State), ?TIMEOUT};
handle_call(get_data, _From, #state{data = Data} = State) ->
    {reply, {ok, Data}, State, ?TIMEOUT};
handle_call({set_data, Data}, _From, State) ->
    {reply, ok, State#state{data = Data},  ?TIMEOUT};
handle_call(close, _From, State) ->
    {reply, ok, close_session(State),  ?TIMEOUT};
handle_call(_Msg, _From, State) ->
    {reply, ignored, State,  ?TIMEOUT}.

handle_cast(shutdown, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State, ?TIMEOUT}.

handle_info(timeout, State) ->
    maybe_close_session(State, erlang:system_time(seconds));
handle_info(_Msg, State) ->
    {noreply, State, ?TIMEOUT}.

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

new_timeout(#state{sess_duration = Duration} = State) ->
    NewTimeout = erlang:system_time(seconds) + Duration,
    State#state{timeout = NewTimeout}.


maybe_close_session(#state{timeout = Timeout} = State, Now)
  when Now >= Timeout ->
    {noreply, close_session(State), 2000};
maybe_close_session(State, _Now) ->
    {noreply, State, ?TIMEOUT}.



to_session(#state{uuid = Id, token = Token, data = Data, timeout = Timeout,
                  skip_cookie = SkipCookie}) ->
    #{pid => self(), uuid => Id, token => Token, data => Data,
      timeout => Timeout, skip_cookie => SkipCookie }.
