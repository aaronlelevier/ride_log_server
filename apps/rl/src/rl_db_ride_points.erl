%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc 'ride_points' represents all points for a ride for all riders
%%% @end
%%% Created : 12. Oct 2020 9:30 AM
%%%-------------------------------------------------------------------
-module(rl_db_ride_points).
-behaviour(rl_db_table).
-author("Aaron Lelevier").
-vsn(1.0).
-include("ride_log.hrl").
-export([name/0, opts/0]).

name() -> ride_points.

opts() -> [
  {attributes, record_info(fields, ride_points)},
  {disc_copies, [node()]},
  {index, [rider_id]},
  {type, bag},
  {storage_properties, [{ets, []}]}
].
