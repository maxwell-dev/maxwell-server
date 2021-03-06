%%%-------------------------------------------------------------------
%%% @author xuchaoqian
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Jun 2018 6:03 PM
%%%-------------------------------------------------------------------
-module(maxwell_server_handler_ext_sup).
-behaviour(supervisor).

%% API
-export([
  start_link/0,
  start_child/1
]).

%% Supervisor callbacks
-export([init/1]).

-define(SUP_NAME, ?MODULE).
-define(SPEC(Module, Args), #{
  id => Module,
  start => {Module, start_link, Args},
  restart => temporary,
  shutdown => 100, % ms
  type => worker,
  modules => [Module]}
).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
  supervisor:start_link({local, ?SUP_NAME}, ?MODULE, []).

start_child(HandlerPid) ->
  supervisor:start_child(?SUP_NAME, [HandlerPid]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  SupFlags = #{strategy => simple_one_for_one, intensity => 0, period => 1},
  ChildSpecs = [?SPEC(maxwell_server_config:get_handler_ext(), [])],
  {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================