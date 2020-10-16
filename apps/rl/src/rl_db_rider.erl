%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc 'rider' table spec
%%%
%%% @end
%%% Created : 12. Oct 2020 9:30 AM
%%%-------------------------------------------------------------------
-module(rl_db_rider).
-behaviour(rl_db_table).
-author("Aaron Lelevier").
-vsn(1.0).
-include("ride_log.hrl").

-export([name/0, opts/0]).

name() -> rider.

opts() -> [
  {attributes, record_info(fields, rider)},
  {disc_copies, [node()]},
  {type, bag},
  {storage_properties, [{ets, []}]}
].
