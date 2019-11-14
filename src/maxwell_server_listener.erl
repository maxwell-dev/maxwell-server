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
  case maxwell_server_config:is_http_enabled() of 
    true -> start_http(Dispatch);
    false -> ignore
  end,
  case maxwell_server_config:is_https_enabled() of 
    true -> start_https(Dispatch);
    false -> ignore
  end.

start_http(Dispatch) ->
  {ok, _} = cowboy:start_clear(
    http,
    [{port, maxwell_server_config:get_port()}],
    #{env => #{dispatch => Dispatch}}
  ).

start_https(Dispatch) -> 
  CertFile = maxwell_server_config:get_cert_file(),
  KeyFile = maxwell_server_config:get_key_file(),
  lager:info("Loading ssl certificate file: ~p", [CertFile]),
  lager:info("Loading ssl key file: ~p", [KeyFile]),
  {ok, _} = cowboy:start_tls(
    https,
    [
      {port, maxwell_server_config:get_ssl_port()},
      {cacertfile, CertFile},
      {certfile, CertFile},
      {keyfile, KeyFile}
    ],
    #{env => #{dispatch => Dispatch}}
  ).