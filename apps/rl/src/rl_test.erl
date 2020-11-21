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

-export([args/0, args/1]).

-spec args() -> map().
args() ->
    args([]).

-spec args(proplists:proplist()) -> map().
args(Opts) ->
    #{% can't override because the 'init' func would need to call 'registration' state functions
      riders => [],
      rider_count => proplists:get_value(rider_count, Opts, 2),
      min_rider_count => proplists:get_value(min_rider_count, Opts, 1),
      max_rider_count => proplists:get_value(max_rider_count, Opts, 2),
      registration_time => proplists:get_value(registration_time, Opts, 1),
      prepare_for_start_time => proplists:get_value(prepare_for_start_time, Opts, 2),
      race_time => proplists:get_value(race_time, Opts, 10),
      points => proplists:get_value(points, Opts, 3)}.
