%%%-------------------------------------------------------------------
%%% @author xuchaoqian
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Mar 2018 3:57 PM
%%%-------------------------------------------------------------------
-module(maxwell_server_registry).
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  dump/0
]).

%% process registry callbacks
-export([
  register_name/2,
  unregister_name/1,
  whereis_name/1,
  send/2
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

-define(SERVER, ?MODULE).

-record(state, {name_refs, ref_names}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

dump() ->
  gen_server:call(?SERVER, dump).

%%%===================================================================
%%% process registry callbacks
%%%===================================================================

register_name(Name, Pid) ->
  gen_server:call(?SERVER, {register_name, {Name, Pid}}).

unregister_name(Name) ->
  gen_server:call(?SERVER, {unregister_name, Name}).

whereis_name(Name) ->
  gen_server:call(?SERVER, {whereis_name, Name}).

send(Name, Msg) ->
  case whereis_name(Name) of
    undefined ->
      exit({badarg, {Name, Msg}});
    Pid ->
      Pid ! Msg,
      Pid
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, init_state()}.

handle_call(dump, _From, State) ->
  reply(dump0(State));
handle_call({register_name, {Name, Pid}}, _From, State) ->
  lager:debug("Registering process: name: ~p, pid: ~p", [Name, Pid]),
  reply(register_name0(Name, Pid, State));
handle_call({unregister_name, Name}, _From, State) ->
  lager:debug("Unregistering process: name: ~p", [Name]),
  reply(unregister_name0(Name, State));
handle_call({whereis_name, Name}, _From, State) ->
  reply(whereis_name0(Name, State));
handle_call(_Request, _From, State) ->
  lager:warning("Received undefined call: ~p", [_Request]),
  reply(ok, State).

handle_cast(_Request, State) ->
  lager:warning("Received undefined cast: ~p", [_Request]),
  noreply(State).

handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
  lager:debug(
    "Monitored process went down: ref: ~p, pid: ~p, reason: ~p",
    [Ref, Pid, Reason]
  ),
  noreply(on_process_down(Ref, State));
handle_info(_Info, State) ->
  lager:warning("Received undefined info: ~p", [_Info]),
  noreply(State).

terminate(_Reason, _State) ->
  lager:info("Terminating registry: reason: ~p", [_Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_state() ->
  #state{name_refs = dict:new(), ref_names = dict:new()}.

dump0(State) ->
  {dump1(State), State}.

dump1(State) ->
  {
    dict:to_list(State#state.name_refs),
    dict:to_list(State#state.ref_names)
  }.

register_name0(Name, Pid, State) ->
  case dict:is_key(Name, State#state.name_refs) of
    true ->
      {no, State};
    false ->
      {yes, register_name1(Name, Pid, State)}
  end.

register_name1(Name, Pid, State) ->
  Ref = erlang:monitor(process, Pid),
  State#state{
    name_refs = dict:store(Name, {Ref, Pid}, State#state.name_refs),
    ref_names = dict:store(Ref, Name, State#state.ref_names)
  }.

unregister_name0(Name, State) ->
  {Name, unregister_name1(Name, State)}.

unregister_name1(Ref, State) when is_reference(Ref) ->
  case dict:find(Ref, State#state.ref_names) of
    {ok, Name} ->
      erlang:demonitor(Ref),
      State#state{
        name_refs = dict:erase(Name, State#state.name_refs),
        ref_names = dict:erase(Ref, State#state.ref_names)
      };
    error ->
      State
  end;
unregister_name1(Name, State) ->
  case dict:find(Name, State#state.name_refs) of
    {ok, {Ref, _Pid}} ->
      erlang:demonitor(Ref),
      State#state{
        name_refs = dict:erase(Name, State#state.name_refs),
        ref_names = dict:erase(Ref, State#state.ref_names)
      };
    error ->
      State
  end.

whereis_name0(Name, State) ->
  {whereis_name1(Name, State), State}.

whereis_name1(Name, State) ->
  case dict:find(Name, State#state.name_refs) of
    {ok, {_Ref, Pid}} ->
      Pid;
    error ->
      undefined
  end.

on_process_down(Ref, State) ->
  unregister_name1(Ref, State).

reply({Reply, State}) ->
  {reply, Reply, State}.

reply(Reply, State) ->
  {reply, Reply, State}.

noreply(State) ->
  {noreply, State}.