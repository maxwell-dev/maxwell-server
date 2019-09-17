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
  get_handler_ext/0, 
  get_port/0
]).

get_port() ->
  {ok, Port} = application:get_env(maxwell_server, port),
  Port.

get_handler_ext() ->
  case application:get_env(maxwell_server, handler_ext) of
    {ok, HandlerExt} -> HandlerExt;
    undefined -> maxwell_server_handler_ext
  end.
