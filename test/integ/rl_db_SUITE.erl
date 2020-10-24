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
-export([all/0, init_per_suite/1, end_per_suite/1]).
%% Tests
-export([ride_insert_and_lookup/1, rider_insert_and_lookup/1,
         ride_point_insert_and_lookup/1, ride_point_lookup_all_ride_points/1]).

%%%===================================================================
%%% API
%%%===================================================================
all() ->
    [ride_insert_and_lookup,
     rider_insert_and_lookup,
     ride_point_insert_and_lookup,
     ride_point_lookup_all_ride_points].

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
    ExpectedRide = ride(Id),

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
    ExpectedRider = rider(Id),

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
    ExpectedRidePoint =
        #{id => Id,
          ride => ride(),
          rider => rider(),
          point_dt => erlang:timestamp(),
          point_name => start,
          point_lat => 38.5743927,
          point_lng => -109.586438},

    % insert RidePoint
    ok = rl_db_ride_point:insert(ExpectedRidePoint),

    % lookup RidePoint
    {ok, RidePoints} = rl_db_ride_point:lookup(Id),
    1 = length(RidePoints),
    [RidePoint | _] = RidePoints,
    RidePoint = ExpectedRidePoint.

ride_point_lookup_all_ride_points(_Config) ->
    % Ride 1
    RiderId1 = rl_util:id(),
    Rider1 = rider(RiderId1),
    RidePoint1 =
        #{id => rl_util:id(),
          ride => ride(),
          rider => Rider1,
          point_dt => erlang:timestamp(),
          point_name => start,
          point_lat => 1,
          point_lng => 2},
    % Ride 2 - is a different ride
    RideId2 = rl_util:id(),
    Ride2 = ride(RideId2),
    Rider2 = rider(),
    RidePoint2 =
        #{id => rl_util:id(),
          ride => Ride2,
          rider => Rider2,
          point_dt => erlang:timestamp(),
          point_name => start,
          point_lat => 1,
          point_lng => 2},
    % insert Ride Points
    ok = rl_db_ride_point:insert(RidePoint1),
    ok = rl_db_ride_point:insert(RidePoint2),

    RidePoints = rl_db_ride_point:lookup_all_ride_points(RideId2),

    % should only get 1 Ride Point back
    true = length(RidePoints) =:= 1,
    % confirm RidePoint matches for Ride 2
    [Rp | _] = RidePoints,
    Ride2 = maps:get(ride, Rp),

    % lookup riders on a ride
    Riders = rl_db_ride_point:lookup_riders(RideId2),
    Riders = [Rider2].

lookup_one_riders_points(_Config) ->
    % Ride 1 - Point 1
    RideId1 = rl_util:id(),
    Ride1 = ride(RideId1),
    RiderId1 = rl_util:id(),
    Rider1 = rider(RiderId1),
    RidePoint1 =
        #{id => rl_util:id(),
          ride => Ride1,
          rider => Rider1,
          point_dt => erlang:timestamp(),
          point_name => start,
          point_lat => 1,
          point_lng => 2},
    % Rider 1 - Point 2
    RidePoint1b =
        #{id => rl_util:id(),
          ride => Ride1,
          rider => Rider1,
          point_dt => erlang:timestamp(),
          point_lat => 2,
          point_lng => 2},
    % Ride 2 - Point 1
    RideId2 = rl_util:id(),
    Ride2 = ride(RideId2),
    Rider2 = rider(),
    RidePoint2 =
        #{id => rl_util:id(),
          ride => Ride2,
          rider => Rider2,
          point_dt => erlang:timestamp(),
          point_name => start,
          point_lat => 1,
          point_lng => 2},
    % insert Ride Points
    ok = rl_db_ride_point:insert(RidePoint1),
    ok = rl_db_ride_point:insert(RidePoint1b),
    ok = rl_db_ride_point:insert(RidePoint2),

    % lookup one rider's points
    RidePoints = rl_db_ride_point:lookup_one_riders_points(RiderId1, RiderId1),

    2 = length(RidePoints),
    true = lists:member(RidePoint1, RidePoints),
    true = lists:member(RidePoint1b, RidePoints).

%%%===================================================================
%%% Internal functions
%%%===================================================================
ride() ->
    ride(rl_util:id()).

ride(Id) ->
    #{id => Id, name => <<"test ride">>}.

rider() ->
    rider(rl_util:id()).

rider(Id) ->
    #{id => Id, name => <<"Aaron">>}.
