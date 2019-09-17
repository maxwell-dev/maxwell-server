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
  handle/2, 
  init/1, 
  terminate/2
]).

-record(state, {}).

%%%===================================================================
%%% Server callbacks
%%%===================================================================
init(_Req) -> #state{}.

handle(_Msg, State) -> noreply(State).

terminate(_Reason, _State) -> ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

noreply(State) -> {noreply, State}.
