%%%-------------------------------------------------------------------
%%% @author xuchaoqian
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jun 2018 11:59 PM
%%%-------------------------------------------------------------------
-module(maxwell_server_listener).

%% API
-export([
  start/0
]).

start() ->
  application:ensure_all_started(cowboy),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", maxwell_server_handler, maxwell_server_handler:initial_state()}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(
    websocket_listener,
    [{port, maxwell_server_config:get_port()}],
    #{env => #{dispatch => Dispatch}}
  ).