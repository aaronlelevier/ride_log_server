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
-export([
  insert/1,
  lookup/1,
  from_item/1,
  to_item/1,
  lookup_all_ride_points/1,
  lookup_riders/1
]).

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------
-type item() :: #{
id => id(),
ride => ride(),
rider => rider(),
point_dt => erlang:timestamp(),
point_name => atom(),
point_lat => float(),
point_lng => float()
}.

-export_type([
  item/0
]).

%%------------------------------------------------------------------------------
%% Behavior rl_db_table
%%------------------------------------------------------------------------------
-behaviour(rl_db_table).
-export([name/0, opts/0]).

name() -> ride_point.

opts() -> [
  {attributes, record_info(fields, ride_point)},
  {disc_copies, [node()]},
  {type, bag},
  {storage_properties, [{ets, []}]}
].

%%%===================================================================
%%% API
%%%===================================================================

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
  Match = ets:fun2ms(
    fun(Rp = #ride_point{ride = Ride})
      when RideId =:= Ride#ride.id ->
      Rp
    end
  ),
  Records = mnesia:dirty_select(ride_point, Match),
  [to_item(R) || R <-  Records].

%% @doc Return all Riders for a single Ride
-spec lookup_riders(id()) -> [rl_db_rider:item()].
lookup_riders(RideId) ->
  Match = ets:fun2ms(
    fun(#ride_point{ride = Ride, rider = Rider})
      when RideId =:= Ride#ride.id ->
      Rider
    end
  ),
  [rl_db_rider:to_item(X) || X <- mnesia:dirty_select(ride_point, Match)].


%%------------------------------------------------------------------------------
%% Item
%%------------------------------------------------------------------------------

-spec from_item(item()) -> ride_point().
from_item(#{
  id := Id,
  ride := Ride,
  rider := Rider,
  point_dt := PointDt,
  point_name := PointName,
  point_lat := PointLat,
  point_lng := PointLng
}) ->
  #ride_point{
    id = Id,
    ride = rl_db_ride:from_item(Ride),
    rider = rl_db_rider:from_item(Rider),
    point_dt = PointDt,
    point_name = PointName,
    point_lat = PointLat,
    point_lng = PointLng
  }.

-spec to_item(ride_point()) -> item().
to_item(#ride_point{
  id = Id,
  ride = Ride,
  rider = Rider,
  point_dt = PointDt,
  point_name = PointName,
  point_lat = PointLat,
  point_lng = PointLng
}) -> #{
  id => Id,
  ride => rl_db_ride:to_item(Ride),
  rider => rl_db_rider:to_item(Rider),
  point_dt => PointDt,
  point_name => PointName,
  point_lat => PointLat,
  point_lng => PointLng
}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
