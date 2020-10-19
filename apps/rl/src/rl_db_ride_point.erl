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

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------
-export([
  insert/1,
  lookup/1,
  from_item/1,
  to_item/1
]).

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------
-record(ride_point, {
  id :: id(),
  ride_id :: id(),
  rider_id :: id(),
  point_dt :: erlang:timestamp(),
  point_name :: atom(),
  point_lat :: float(),
  point_lng :: float()
}).

-type ride_point() :: #ride_point{}.

-type item() :: #{
id => id(),
ride_id => id(),
rider_id => id(),
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
  {index, [rider_id]},
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

-spec from_item(map()) -> ride_point().
from_item(#{
  id := Id,
  ride_id := RideId,
  rider_id := RiderId,
  point_dt := PointDt,
  point_name := PointName,
  point_lat := PointLat,
  point_lng := PointLng
}) ->
  #ride_point{
    id = Id,
    ride_id = RideId,
    rider_id = RiderId,
    point_dt = PointDt,
    point_name = PointName,
    point_lat = PointLat,
    point_lng = PointLng
  }.

-spec to_item(ride_point()) -> map().
to_item(#ride_point{
  id = Id,
  ride_id = RideId,
  rider_id = RiderId,
  point_dt = PointDt,
  point_name = PointName,
  point_lat = PointLat,
  point_lng = PointLng
}) -> #{
  id => Id,
  ride_id => RideId,
  rider_id => RiderId,
  point_dt => PointDt,
  point_name => PointName,
  point_lat => PointLat,
  point_lng => PointLng
}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
