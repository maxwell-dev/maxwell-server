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
  get_tls_key_dir/0,
  get_handler_ext/0
]).

get_port() ->
  {ok, Port} = application:get_env(maxwell_server, port),
  Port.

get_tls_key_dir() ->
  case application:get_env(maxwell_server, tls_key_dir) of
    {ok, TlsKeyDir} -> TlsKeyDir;
    undefined -> code:priv_dir(maxwell_server)
  end.

get_handler_ext() ->
  case application:get_env(maxwell_server, handler_ext) of
    {ok, HandlerExt} -> HandlerExt;
    undefined -> maxwell_server_handler_ext
  end.
