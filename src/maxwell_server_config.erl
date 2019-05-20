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
  get_node_id/0,
  get_public_ip/0,
  get_private_ip/0,
  get_port/0,
  get_public_endpoint/0,
  get_private_endpoint/0,
  get_handler_ext/0
]).

get_node_id() ->
  case init:get_argument(node_id) of
    error ->
      exit(no_node_id);
    {ok, [[Value]]} ->
      erlang:list_to_integer(Value)
  end.

get_public_ip() ->
  case init:get_argument(public_ip) of
    error -> exit(no_public_ip);
    {ok, [[Value]]} -> Value
  end.

get_private_ip() ->
  case init:get_argument(private_ip) of
    error -> exit(no_private_ip);
    {ok, [[Value]]} -> Value
  end.

get_port() ->
  {ok, Port} = application:get_env(maxwell_server, port),
  Port.

get_public_endpoint() ->
  lists:concat([get_public_ip(), ":", get_port()]).

get_private_endpoint() ->
  lists:concat([get_private_ip(), ":", get_port()]).

get_handler_ext() ->
  case application:get_env(maxwell_server, handler_ext) of
    {ok, HandlerExt} -> HandlerExt;
    undefined -> maxwell_server_handler_ext
  end.

