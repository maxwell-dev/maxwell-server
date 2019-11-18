%%%-------------------------------------------------------------------
%%% @author xuchaoqian
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. May 2018 7:33 PM
%%%-------------------------------------------------------------------
-module(maxwell_server_handler).

-include_lib("maxwell_protocol/include/maxwell_protocol_pb.hrl").

%% API
-export([
  initial_state/0,
  send/2
]).

%% Cowboy callbacks
-export([
  init/2,
  websocket_init/1,
  websocket_handle/2,
  websocket_info/2,
  terminate/3
]).


-define(SEND_CMD(Msg), {'$send', Msg}).

-record(state, {handler_ext, peer_endpoint, state_ext}).

%%%===================================================================
%%% API
%%%===================================================================

initial_state() ->
  HandlerExt = maxwell_server_config:get_handler_ext(),
  #state{handler_ext = HandlerExt}.

send(Pid, Msg) -> 
  Pid ! ?SEND_CMD(Msg).

%%%===================================================================
%%% Cowboy callbacks
%%%===================================================================

init(Req, State) ->
  lager:debug("Initializing handler: req: ~p, state: ~p", [Req, State]),
  {cowboy_websocket,
    Req,
    {Req, State},
    #{idle_timeout => 120000, max_frame_size => 16777216}
  }.

websocket_init({Req, State}) ->
  lager:debug("Initializing websocket: state: ~p", [State]),
  #{headers := Headers, peer := Endpoint} = Req,
  Agent = case maps:find(<<"user-agent">>, Headers) of
            {ok, Value} -> Value;
            error -> #{}
          end,
  Endpoint2 = convert_endpoint(Endpoint),
  HandlerExt = State#state.handler_ext,
  StateExt = HandlerExt:init(#{agent=>Agent, endpoint=>Endpoint2}),
  noreply(State#state{peer_endpoint = Endpoint2, state_ext = StateExt}).

websocket_handle({binary, EncodedMsg}, State) ->
  Msg = recv(EncodedMsg, State),
  handle(Msg, State);
websocket_handle(Msg, State) ->
  lager:debug("Ignored msg: ~p, from: ~p", [Msg, State#state.peer_endpoint]),
  noreply(State).

websocket_info(?SEND_CMD(Msg), State) ->
  reply(Msg, State);
websocket_info(Msg, State) ->
  handle(Msg, State).

terminate(Reason, Req, State) ->
  lager:debug(
    "Terminating handler: req: ~p, reason: ~p, state: ~p",
    [Req, Reason, State]
  ),
  HandlerExt = State#state.handler_ext,
  HandlerExt:terminate(Reason, State#state.state_ext),
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

convert_endpoint(Endpoint) ->
  {{A, B, C, D}, Port} = Endpoint,
  list_to_binary(io_lib:format("~w.~w.~w.~w:~w", [A, B, C, D, Port])).

recv(EncodedMsg, State) ->
  Msg = maxwell_protocol:decode_msg(EncodedMsg),
  lager:debug("Received msg: ~p, from: ~p", [Msg, State#state.peer_endpoint]),
  Msg.

handle(#ping_req_t{}, State) ->
  reply(#ping_rep_t{}, State);
handle(Msg, State) ->
  HandlerExt = State#state.handler_ext,
  case HandlerExt:handle(Msg, State#state.state_ext) of
    {reply, Reply, StateExt} ->
      reply(Reply, State#state{state_ext = StateExt});
    {noreply, StateExt} ->
      noreply(State#state{state_ext = StateExt});
    {stop, _, StateExt} ->
      stop(State#state{state_ext = StateExt})
  end.

reply(Reply, State) ->
  lager:debug("Sending msg: ~p, to: ~p", [Reply, State#state.peer_endpoint]),
  EncodedMsg = maxwell_protocol:encode_msg(Reply),
  {[{binary, EncodedMsg}], State}.

noreply(State) ->
  {ok, State}.

stop(State) ->
  {stop, State}.