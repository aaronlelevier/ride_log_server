%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc 'ride' table spec
%%%
%%% @end
%%% Created : 12. Oct 2020 9:30 AM
%%%-------------------------------------------------------------------
-module(rl_db_ride).
-behaviour(rl_db_table).
-author("Aaron Lelevier").
-vsn(1.0).
-export([name/0, opts/0]).
-export_type([ride/0]).

name() -> ride.

-record(ride, {
  ride_id,
  name
}).
-type ride() :: #ride{}.

opts() -> [
  {attributes, record_info(fields, ride)},
  {disc_copies, [node()]},
  {type, bag},
  {storage_properties, [{ets, []}]}
].
