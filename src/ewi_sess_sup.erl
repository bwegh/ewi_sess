%% @private
-module(ewi_sess_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
    SupFlags = #{ strategy => one_for_one,
                 intensity => 10,
                 period => 10
               },
    Procs = [ session_sup(),
              session_manager()
            ],
    {ok, {SupFlags, Procs}}.


session_manager() ->
    #{ id => session_manager,
       start => {ewi_sess_mgr, start_link, []}
     }.

session_sup() ->
    #{ id => session_sup,
       start => {ewi_sess_session_sup, start_link, []},
       type => supervisor
     }.
