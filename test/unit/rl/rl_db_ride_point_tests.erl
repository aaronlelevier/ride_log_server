%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 22. Oct 2020 6:42 AM
%%%-------------------------------------------------------------------
-module(rl_db_ride_point_tests).

-author("Aaron Lelevier").

-include_lib("eunit/include/eunit.hrl").

-define(RIDE, #{id => rl_util:id(), name => <<"test ride">>}).

-define(RIDER, #{id => rl_util:id(), name => <<"Aaron">>}).

-define(POINT_NAME, start).

-define(POINT_LAT, 31.5743927).

-define(POINT_LNG, 100.1234392).


create_ride_point_with_point_name_test() ->
    NewRidePoint = #{
        ride => ?RIDE,
        rider => ?RIDER,
        point_name => ?POINT_NAME,
        point_lat => ?POINT_LAT,
        point_lng => ?POINT_LNG
    },

    Ret = rl_db_ride_point:create(NewRidePoint),

    ?assert(is_map(Ret)).

create_ride_point_without_point_name_test() ->
    NewRidePoint = #{
        ride => ?RIDE,
        rider => ?RIDER,
        point_lat => ?POINT_LAT,
        point_lng => ?POINT_LNG
    },

    Ret = rl_db_ride_point:create(NewRidePoint),

    ?assert(is_map(Ret)).

from_item_test() ->
    RideId = rl_util:id(),
    RiderId = rl_util:id(),
    RidePointId = rl_util:id(),
    Ride = #{id => RideId, name => <<"test ride">>},
    Rider = #{id => RiderId, name => <<"Aaron">>},
    RidePoint =
        #{id => RidePointId,
          ride => Ride,
          rider => Rider,
          point_dt => {1603, 375581, 441019}, % normally ~ erlang:timestamp(),
          point_name => start,
          point_lat => 38.5743927,
          point_lng => -109.586438},

    Ret = rl_db_ride_point:from_item(RidePoint),

    ?assertEqual({ride_point,
                  RidePointId,
                  {ride, RideId, <<"test ride">>},
                  {rider, RiderId, <<"Aaron">>},
                  {1603, 375581, 441019},
                  start,
                  38.5743927,
                  -109.586438},
                 Ret),

    Ret2 = rl_db_ride_point:to_item(Ret),

    ?assertEqual(RidePoint, Ret2).
