%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2020 10:06 AM
%%%-------------------------------------------------------------------
-module(rl_client).
-author("Aaron Lelevier").
-vsn(1.0).
-export([start/0]).

%% @doc A Rider starts a new Ride
start() ->
  {ok, RideId} = rl_ride:start_link(),
  {ok, RiderId} = rl_rider:start_link(),

  lager:debug("riders: ~p", [rl_ride:list_riders(RideId)]),
  lager:debug("is_riding: ~p", [rl_rider:is_riding(RiderId)]),
  lager:debug("ride_info: ~p", [rl_rider:ride_info(RiderId)]),

  % Rider joins a Ride - blocking - must wait until Ride confirms
  rl_rider:join_ride(RiderId, RideId),

  lager:debug("riders: ~p", [rl_ride:list_riders(RideId)]),
  lager:debug("is_riding: ~p", [rl_rider:is_riding(RiderId)]),
  lager:debug("ride_info: ~p", [rl_rider:ride_info(RiderId)]),

  {RideId, RiderId}.
