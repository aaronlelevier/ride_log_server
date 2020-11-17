%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Module for test helpers, so they can be re-used
%%% @end
%%% TODO: will need to exclude this from the "prod" app
%%% Created : 17. Nov 2020 6:46 AM
%%%-------------------------------------------------------------------
-module(rl_test).
-author("Aaron Lelevier").
-vsn(1.0).
-export([args/0]).

args() -> #{
    rider_count => 0,
    riders => [],
    min_rider_count => 1,
    max_rider_count => 2,
    registration_time => 1,
    prepare_for_start_time => 2,
    race_time => 10,
    points => 3
}.