%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 18. Oct 2020 9:09 AM
%%%-------------------------------------------------------------------
-module(rl_db_SUITE).
-author("Aaron Lelevier").
-vsn(1.0).

%% CT
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

%% Tests
-export([
  ride_insert_and_lookup/1,
  rider_insert_and_lookup/1,
  ride_point_insert_and_lookup/1
]).

%%%===================================================================
%%% API
%%%===================================================================
all() ->
  [
    ride_insert_and_lookup,
    rider_insert_and_lookup,
    ride_point_insert_and_lookup
  ].

%%==============================================================================
%% Types
%%==============================================================================
-type config() :: [{atom(), any()}].

%%------------------------------------------------------------------------------
%% CT
%%------------------------------------------------------------------------------
-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  ok = rl_db:start(),
  Config.

-spec end_per_suite(config()) -> ok.
end_per_suite(_Config) ->
  ok = rl_db:stop(),
  ok.

%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------
ride_insert_and_lookup(_Config) ->
  % ride info
  Id = rl_util:id(),
  ExpectedRide = #{id => Id, name => <<"test ride">>},

  % insert Ride
  ok = rl_db_ride:insert(ExpectedRide),

  % lookup Ride
  {ok, Rides} = rl_db_ride:lookup(Id),
  1 = length(Rides),
  [Ride | _] = Rides,
  Ride = ExpectedRide.

rider_insert_and_lookup(_Config) ->
  % ride info
  Id = rl_util:id(),
  ExpectedRider = #{id => Id, name => <<"Aaron">>},

  % insert Rider
  ok = rl_db_rider:insert(ExpectedRider),

  % lookup Rider
  {ok, Riders} = rl_db_rider:lookup(Id),
  1 = length(Riders),
  [Rider | _] = Riders,
  Rider = ExpectedRider.


ride_point_insert_and_lookup(_Config) ->
  % ride info
  Id = rl_util:id(),
  ExpectedRidePoint = #{
    id => Id,
    ride_id => rl_util:id(),
    rider_id => rl_util:id(),
    point_dt => erlang:timestamp(),
    point_name => start,
    point_lat => 38.5743927,
    point_lng => -109.586438
  },

  % insert RidePoint
  ok = rl_db_ride_point:insert(ExpectedRidePoint),

  % lookup RidePoint
  {ok, RidePoints} = rl_db_ride_point:lookup(Id),
  1 = length(RidePoints),
  [RidePoint | _] = RidePoints,
  RidePoint = ExpectedRidePoint.