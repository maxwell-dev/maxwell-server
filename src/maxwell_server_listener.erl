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
  TlsKeyDir = maxwell_server_config:get_tls_key_dir(),
  lager:info("Loading tls key from dir: ~p", [TlsKeyDir]),
  {ok, _} = cowboy:start_tls(
    https,
    [
      {port, maxwell_server_config:get_port()},
      {cacertfile, TlsKeyDir ++ "/tls.pem"},
		  {certfile, TlsKeyDir ++ "/tls.pem"},
		  {keyfile, TlsKeyDir ++ "/tls.key"}
    ],
    #{env => #{dispatch => Dispatch}}
  ).