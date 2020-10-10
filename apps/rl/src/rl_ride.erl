%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(rl_ride).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% Macros
-define(SERVER, ?MODULE).
-define(INIT_STATE, #{
  riders => []
}).

-compile(export_all).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link(?MODULE, [], []).

add_rider(RideId, RiderId) ->
  gen_server:call(RideId, {add_rider, RiderId}).

list_riders(RideId) ->
  gen_server:call(RideId, list_riders).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

init([]) ->
  {ok, ?INIT_STATE}.

handle_call({add_rider, RiderId}, _From, State0) ->
  State = State0#{
    riders := [RiderId | maps:get(riders, State0)]
  },
  {reply, ok, State};
handle_call(list_riders, _From, State) ->
  Reply = maps:get(riders, State),
  {reply, Reply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
