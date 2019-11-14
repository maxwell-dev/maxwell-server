%%%-------------------------------------------------------------------
%%% @author xuchaoqian
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jun 2018 6:13 PM
%%%-------------------------------------------------------------------
-module(maxwell_server_config).

%% API
-export([
  get_port/0,
  get_ssl_port/0,
  get_cert_file/0,
  get_key_file/0,
  get_handler_ext/0,
  is_http_enabled/0,
  is_https_enabled/0
]).

get_port() ->
  {ok, Port} = application:get_env(maxwell_server, http_port),
  Port.

get_ssl_port() ->
  {ok, Port} = application:get_env(maxwell_server, https_port),
  Port.

get_cert_file() ->
  case application:get_env(maxwell_server, cert_file) of
    {ok, CertFile} -> CertFile;
    undefined -> code:priv_dir(maxwell_server) ++ "/ssl.pem"
  end.

get_key_file() ->
  case application:get_env(maxwell_server, key_file) of
    {ok, KeyFile} -> KeyFile;
    undefined -> code:priv_dir(maxwell_server) ++ "/ssl.key"
  end.

get_handler_ext() ->
  case application:get_env(maxwell_server, handler_ext) of
    {ok, HandlerExt} -> HandlerExt;
    undefined -> maxwell_server_handler_ext
  end.

is_http_enabled() -> 
  case application:get_env(maxwell_server, http_port) of 
    {ok, _} -> true;
    undefined -> false
  end.

is_https_enabled() -> 
  case application:get_env(maxwell_server, https_port) of 
    {ok, _} -> true;
    undefined -> false
  end.