%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(rl_rider).
-behaviour(gen_server).

%% API
-export([start_link/0, join_ride/2, is_riding/1, ride_info/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% Macros
-define(SERVER, ?MODULE).
-define(INIT_STATE, #{
  ride => undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link(?MODULE, [], []).

%% @doc Rider begins a Ride
%% this should POST the Rider's first "start point" entry for the Ride
join_ride(RiderId, RideId) ->
  lager:debug("join_ride RiderId:~p", [RiderId]),
  gen_server:call(RiderId, {join_ride, RideId}).

is_riding(RiderId) ->
  gen_server:call(RiderId, is_riding).

ride_info(RiderId) ->
  gen_server:call(RiderId, ride_info).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

init([]) ->
  {ok, ?INIT_STATE}.

handle_call({join_ride, RideId}, _From, State0) ->
  lager:debug("handle_call join_ride RiderId:~p", [self()]),
  ok = rl_ride:add_rider(RideId, self()),
  State = State0#{ride := RideId},
  {reply, ok, State};
handle_call(is_riding, _From, State) ->
  Reply = maps:get(ride, State) =/= undefined,
  {reply, Reply, State};
handle_call(ride_info, _From, State) ->
  Reply = maps:get(ride, State),
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
