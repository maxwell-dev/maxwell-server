%%%-------------------------------------------------------------------
%%% @author xuchaoqian
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jun 2018 5:35 PM
%%%-------------------------------------------------------------------
-module(maxwell_server_handler_ext).

-include_lib("maxwell_protocol/include/maxwell_protocol_pb.hrl").

-export([
  init/1,
  pre_pull/2,
  pre_push/2,
  handle/2,
  terminate/2
]).

-record(state, {}).

%%%===================================================================
%%% Server callbacks
%%%===================================================================
init(_Req) ->
  #state{}.

pre_pull(_Msg, _State) ->
  ok.
pre_push(_Msg, _State) ->
  ok.

handle(_Msg, State) ->
  noreply(State).

terminate(_Reason, _State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

noreply(State) ->
  {noreply, State}.