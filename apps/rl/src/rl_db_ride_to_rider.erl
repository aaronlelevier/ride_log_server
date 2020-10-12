%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc 'ride_to_rider' table spec
%%%
%%% @end
%%% Created : 12. Oct 2020 9:30 AM
%%%-------------------------------------------------------------------
-module(rl_db_ride_to_rider).
-behaviour(rl_db_table).
-author("Aaron Lelevier").
-vsn(1.0).
-export([name/0, opts/0]).
-export_type([ride_to_rider/0]).

name() -> ride_to_rider.

-record(ride_to_rider, {
  ride_id,
  rider_id
}).
-type ride_to_rider() :: #ride_to_rider{}.

opts() -> [
  {attributes, record_info(fields, ride_to_rider)},
  {disc_copies, [node()]},
  {index, [rider_id]},
  {type, bag},
  {storage_properties, [{ets, []}]}
].
