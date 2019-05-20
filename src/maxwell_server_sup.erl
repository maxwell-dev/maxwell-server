%%%-------------------------------------------------------------------
%%% @author xuchaoqian
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Mar 2018 6:00 PM
%%%-------------------------------------------------------------------
-module(maxwell_server_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SUP_NAME, ?MODULE).
-define(SPEC(Module, Type, Args), #{
  id => Module,
  start => {Module, start_link, Args},
  restart => permanent,
  shutdown => infinity,
  type => Type,
  modules => [Module]}
).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?SUP_NAME}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  SupFlags = #{strategy => one_for_one, intensity => 10, period => 60},
  ChildSpecs = [
    ?SPEC(maxwell_server_registry, worker, []),
    ?SPEC(maxwell_server_pusher_sup, supervisor, []),
    ?SPEC(maxwell_server_puller_sup, supervisor, []),
    ?SPEC(maxwell_server_handler_ext_sup, supervisor, [])
  ],
  {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================