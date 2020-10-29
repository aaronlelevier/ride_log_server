%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 29. Oct 2020 6:44 AM
%%%-------------------------------------------------------------------
-module(rl_race_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").


%%%===================================================================
%%% Tests
%%%===================================================================
a_race_can_be_started_and_cancelled_test() ->
  Race = race0,
  Args = args(),
  {ok, _Pid} = rl_race:start_link(Race, Args),

  % initial state
  {StateName1, State1} = rl_race:get_state(Race),

  ?assertEqual(registration, StateName1),
  ?assertEqual(Args, State1),

  % cancel race
  {StateName2, State2} = rl_race:cancel(Race),

  ?assertEqual(cancelled, StateName2),
  ?assertEqual(Args, State2).

register_rider_adds_a_rider_to_the_race_test() ->
  Race = race1,
  Args = args(),
  {ok, _Pid} = rl_race:start_link(Race, Args),

  {StateName1, State1} = rl_race:get_state(Race),

  % start with 0 Riders
  ?assertEqual(registration, StateName1),
  ?assertEqual(0, maps:get(rider_count, State1)),
  ?assertEqual([], maps:get(riders, State1)),

  Rider = #{id => rl_util:id(), name => <<"Bob">>},
  {StateName1, State1} = rl_race:register_rider(Race, Rider),

  {StateName1, State2} = rl_race:get_state(Race),

  % now there's 1 Rider
  ?assertEqual(registration, StateName1),
  ?assertEqual(1, maps:get(rider_count, State2)),
  ?assertEqual([Rider], maps:get(riders, State2)).

%% TODO:
%%cancel_race_if_N_riders_not_registered_in_T_time_test() ->


%%%===================================================================
%%% Internal functions
%%%===================================================================
args() -> #{
  rider_count => 0,
  riders => [],
  min_rider_count => 1,
  max_rider_count => 2,
  registration_time => 2,
  prepare_for_start_time => 2,
  race_time => 10,
  points => 3
}.