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
    http,
    [{port, maxwell_server_config:get_port()}],
    #{env => #{dispatch => Dispatch}}
  ),

  CertDir = maxwell_server_config:get_cert_dir(),
  lager:info("Loading ssl certificate from dir: ~p", [CertDir]),
  {ok, _} = cowboy:start_tls(
    https,
    [
      {port, maxwell_server_config:get_ssl_port()},
      {cacertfile, CertDir ++ "/ssl.pem"},
		  {certfile, CertDir ++ "/ssl.pem"},
		  {keyfile, CertDir ++ "/ssl.key"}
    ],
    #{env => #{dispatch => Dispatch}}
  ).