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
  get_cert_dir/0,
  get_handler_ext/0
]).

get_port() ->
  {ok, Port} = application:get_env(maxwell_server, port),
  Port.

get_ssl_port() ->
  {ok, Port} = application:get_env(maxwell_server, ssl_port),
  Port.

get_cert_dir() ->
  case application:get_env(maxwell_server, cert_dir) of
    {ok, CertDir} -> string:trim(CertDir, trailing, "/");
    undefined -> code:priv_dir(maxwell_server)
  end.

get_handler_ext() ->
  case application:get_env(maxwell_server, handler_ext) of
    {ok, HandlerExt} -> HandlerExt;
    undefined -> maxwell_server_handler_ext
  end.
