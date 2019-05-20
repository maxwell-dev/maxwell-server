%%%-------------------------------------------------------------------
%%% @author xuchaoqian
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jun 2018 5:35 PM
%%%-------------------------------------------------------------------
-module(maxwell_server_puller).
-behaviour(gen_server).

-include_lib("basin/include/basin.hrl").
-include_lib("maxwell_protocol/include/maxwell_protocol_pb.hrl").

%% API
-export([
  start_link/2,
  ensure_started/2,
  pull/2
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(PROCESS_NAME(Topic, HandlerPid), {puller, Topic, HandlerPid}).
-define(VIA_PROCESS_NAME(Topic, HandlerPid),
  {via, maxwell_server_registry, ?PROCESS_NAME(Topic, HandlerPid)}
).

-record(state, {
  topic,
  basin_ref,
  basin_pid,
  handler_ref,
  handler_pid,
  pending_pull_req
}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Topic, HandlerPid) ->
  gen_server:start_link(
    ?VIA_PROCESS_NAME(Topic, HandlerPid),
    ?MODULE, [Topic, HandlerPid], []).

ensure_started(Topic, HandlerPid) ->
  case maxwell_server_registry:whereis_name(
    ?PROCESS_NAME(Topic, HandlerPid)) of
    undefined ->
      case maxwell_server_puller_sup:start_child(
        Topic, HandlerPid) of
        {error, {already_started, Pid}} -> {ok, Pid};
        {ok, _} = Result -> Result
      end;
    Pid -> {ok, Pid}
  end.

pull(Pid, Msg) ->
  gen_server:cast(Pid, {pull, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Topic, HandlerPid]) ->
  State = init_state(Topic, HandlerPid),
  lager:info(
    "Initializing ~p: state: ~p", [?MODULE, State]
  ),
  {ok, State}.

handle_call(Request, _From, State) ->
  lager:error("Received unknkonw call: ~p", [Request]),
  reply({ok, State}).

handle_cast({pull, Msg}, State) ->
  noreply(pull0(Msg, State));
handle_cast(Request, State) ->
  lager:error("Received unknkonw cast: ~p", [Request]),
  noreply(State).

handle_info(?BASIN_NOTIFY_CMD(MaxOffset), State) ->
  lager:debug("~p", [?BASIN_NOTIFY_CMD(MaxOffset)]),
  noreply(notify(State));
handle_info({'DOWN', BasinRef, process, _BasinPid, Reason},
    #state{basin_ref = BasinRef} = State) ->
  lager:info(
    "Basin was down: state: ~p, reason: ~p", [State, Reason]
  ),
  noreply(init_basin(State));
handle_info({'DOWN', HandlerRef, process, _HandlerPid, Reason},
    #state{handler_ref = HandlerRef} = State) ->
  lager:info(
    "Handler was down: state: ~p, reason: ~p", [State, Reason]
  ),
  stop({{error, handler_down}, State});
handle_info(Info, State) ->
  lager:error("Received unknkonw info: ~p", [Info]),
  noreply(State).

terminate(Reason, State) ->
  lager:info(
    "Terminating ~p: state: ~p, reason: ~p",
    [?MODULE, State, Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_state(Topic, HandlerPid) ->
  State = #state{
    topic = Topic,
    handler_pid = HandlerPid,
    pending_pull_req = undefined
  },
  monitor_handler(init_basin(State)).

init_basin(State) ->
  {BasinRef, BasinPid} = get_basin(State#state.topic),
  basin_topic_owner:add_watcher(BasinPid, self()),
  State#state{basin_ref = BasinRef, basin_pid = BasinPid}.

get_basin(Topic) ->
  {ok, Pid} = basin_topic_owner:ensure_started(Topic),
  Ref = erlang:monitor(process, Pid),
  {Ref, Pid}.

monitor_handler(State) ->
  Ref = erlang:monitor(process, State#state.handler_pid),
  State#state{handler_ref = Ref}.

pull0(#pull_req_t{
  offset = Offset, limit = Limit, ref = Ref
} = PullReq, State) ->
  Entries = basin_topic_owner:get_from(
    State#state.basin_pid, Offset, Limit
  ),
  case erlang:length(Entries) > 0 of
    true ->
      Rep = #pull_rep_t{
        msgs = build_msgs(Entries),
        ref = Ref
      },
      maxwell_server_handler:send(
        State#state.handler_pid, Rep
      ),
      State#state{pending_pull_req = undefined};
    false ->
      State#state{pending_pull_req = PullReq}
  end.

build_msgs(Entries) ->
  lists:reverse(lists:foldl(
    fun({Offset, Value, Timestamp}, Msgs) ->
      Msg = #msg_t{
        offset = Offset,
        value = Value,
        timestamp = Timestamp
      },
      [Msg | Msgs]
    end, [], Entries)).

notify(State) ->
  case State#state.pending_pull_req of
    undefined -> State;
    PullReq -> pull0(PullReq, State)
  end.

reply({Reply, State}) ->
  {reply, Reply, State}.

noreply(State) ->
  {noreply, State}.

stop({Reason, State}) ->
  {stop, Reason, State}.