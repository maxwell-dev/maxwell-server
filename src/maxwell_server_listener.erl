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
  PrivDir = code:priv_dir(maxwell_server),
  lager:info("Loaded ca from dir: ~p", [PrivDir]),
  {ok, _} = cowboy:start_tls(
    https,
    [
      {port, maxwell_server_config:get_port()},
      {cacertfile, PrivDir ++ "/ssl/cowboy-ca.crt"},
		  {certfile, PrivDir ++ "/ssl/server.crt"},
		  {keyfile, PrivDir ++ "/ssl/server.key"}
    ],
    #{env => #{dispatch => Dispatch}}
  ).