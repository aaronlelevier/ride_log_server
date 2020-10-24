%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 22. Oct 2020 6:42 AM
%%%-------------------------------------------------------------------
-module(rl_db_ride_point_tests).

-author("Aaron Lelevier").

-include_lib("eunit/include/eunit.hrl").

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
