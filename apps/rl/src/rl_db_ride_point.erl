%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc 'ride_point' represents a Point on a Ride for a Rider
%%% @end
%%% Created : 12. Oct 2020 9:30 AM
%%%-------------------------------------------------------------------
-module(rl_db_ride_point).

-author("Aaron Lelevier").

-vsn(1.0).

-include("ride_log.hrl").

-include_lib("stdlib/include/ms_transform.hrl").

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------
-export([create/1, insert/1, lookup/1, from_item/1, to_item/1, lookup_all_ride_points/1,
         lookup_riders/1, lookup_one_riders_points/2]).

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------
-type item() :: ride_point().

-export_type([item/0]).

%% @doc This is the RidePoint data sent by the client and does not contain
%% calculated fields
-type new_ride_point() :: #{ride => ride(),
                            rider => rider(),
                            point_name => atom() | undefined,
                            point_lat => float(),
                            point_lng => float()}.

%%------------------------------------------------------------------------------
%% Behavior rl_db_table
%%------------------------------------------------------------------------------
-behaviour(rl_db_table).

-export([name/0, opts/0]).

name() ->
    ride_point.

opts() ->
    [{attributes, record_info(fields, ride_point)},
     {disc_copies, [node()]},
     {type, bag},
     {storage_properties, [{ets, []}]}].

%%%===================================================================
%%% API
%%%===================================================================

-spec create(new_ride_point()) -> item().
create(Map) when is_map(Map) ->
    #{% calculated
      id => rl_util:id(),
      point_dt => calendar:local_time(),
      % required
      ride => maps:get(ride, Map),
      rider => maps:get(rider, Map),
      point_lat => maps:get(point_lat, Map),
      point_lng => maps:get(point_lng, Map),
      % optional
      point_name => maps:get(point_name, Map, undefined)}.

-spec insert(item()) -> ok | {error, any()}.
insert(Map) when is_map(Map) ->
    Record = from_item(Map),
    rl_db:write(Record).

-spec lookup(id()) -> {ok, [item()]}.
lookup(Id) ->
    {ok, Items} = rl_db:lookup(name(), Id),
    {ok, [to_item(Item) || Item <- Items]}.

%% @doc Returns all Ride Points for a single Ride
-spec lookup_all_ride_points(id()) -> {ok, [item()]}.
lookup_all_ride_points(RideId) ->
    Match =
        ets:fun2ms(fun (Rp = #ride_point{ride = Ride}) when RideId =:= Ride#ride.id ->
                           Rp
                   end),
    [to_item(R) || R <- mnesia:dirty_select(ride_point, Match)].

%% @doc Return all Riders for a single Ride
-spec lookup_riders(id()) -> [rl_db_rider:item()].
lookup_riders(RideId) ->
    Match =
        ets:fun2ms(fun (#ride_point{ride = Ride, rider = Rider}) when RideId =:= Ride#ride.id ->
                           Rider
                   end),
    [rl_db_rider:to_item(X) || X <- mnesia:dirty_select(ride_point, Match)].

%% @doc Return all Ride Points for a single Rider on a Ride
-spec lookup_one_riders_points(id(), id()) -> [rl_db_ride_point:item()].
lookup_one_riders_points(RideId, RiderId) ->
    Match =
        ets:fun2ms(fun (Rp = #ride_point{ride = Ride, rider = Rider})
                           when RideId =:= Ride#ride.id andalso RiderId =:= Rider#rider.id ->
                           Rp
                   end),
    [to_item(R) || R <- mnesia:dirty_select(ride_point, Match)].

%%------------------------------------------------------------------------------
%% Item
%%------------------------------------------------------------------------------

-spec from_item(item()) -> #ride_point{}.
from_item(#{id := Id,
            ride := Ride,
            rider := Rider,
            point_dt := PointDt,
            point_name := PointName,
            point_lat := PointLat,
            point_lng := PointLng}) ->
    #ride_point{id = Id,
                ride = rl_db_ride:from_item(Ride),
                rider = rl_db_rider:from_item(Rider),
                point_dt = PointDt,
                point_name = PointName,
                point_lat = PointLat,
                point_lng = PointLng}.

-spec to_item(#ride_point{}) -> item().
to_item(#ride_point{id = Id,
                    ride = Ride,
                    rider = Rider,
                    point_dt = PointDt,
                    point_name = PointName,
                    point_lat = PointLat,
                    point_lng = PointLng}) ->
    #{id => Id,
      ride => rl_db_ride:to_item(Ride),
      rider => rl_db_rider:to_item(Rider),
      point_dt => PointDt,
      point_name => PointName,
      point_lat => PointLat,
      point_lng => PointLng}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
