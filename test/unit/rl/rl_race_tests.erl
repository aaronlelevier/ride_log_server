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
  {StateName1, _State1} = rl_race:get_state(Race),
  ?assertEqual(registration, StateName1),

  % cancel race
  {StateName2, _State2} = rl_race:cancel(Race),
  ?assertEqual(cancelled, StateName2).

race_cancelled_if_N_riders_not_registered_in_T_time_test() ->
  RegistrationTime = 0,
  Race = race1,
  Args = #{
    rider_count => 0,
    riders => [],
    min_rider_count => 1,
    max_rider_count => 2,
    registration_time => RegistrationTime,
    prepare_for_start_time => 2,
    race_time => 10,
    points => 3
  },
  {ok, _Pid} = rl_race:start_link(Race, Args),

  % initial state
  {StateName1, State} = rl_race:get_state(Race),
  true = maps:get(rider_count, State) < maps:get(min_rider_count, State),
  ?assertEqual(cancelled, StateName1).


%%%===================================================================
%%% setup / cleanup for multi FSM tests
%%%===================================================================
-define(RACE, race2).

register_rider_setup() ->
  Race = ?RACE,
  Args = args(),
  {ok, _Pid} = rl_race:start_link(Race, Args).

register_rider_cleanup(_) ->
  rl_race:stop(?RACE).

register_rider_test_() ->
  {setup,
    fun register_rider_setup/0,
    fun register_rider_cleanup/1,
    [
      fun start_with_0_riders/0,
      fun start_shows_state_name_in_state/0,
      fun register_1_rider/0,
      fun min_rider_count_satisfied_so_race_not_cancelled/0,
      fun max_rider_count_reached_so_transition_to_registration_full/0
    ]
  }.

start_with_0_riders() ->
  {StateName1, State1} = rl_race:get_state(?RACE),
  ?assertEqual(registration, StateName1),
  ?assertEqual(0, maps:get(rider_count, State1)),
  ?assertEqual([], maps:get(riders, State1)).

start_shows_state_name_in_state() ->
  {StateName, State} = rl_race:get_state(?RACE),
  ?assertEqual(registration, StateName),
  ?assertEqual(registration, maps:get(state_name, State)).

register_1_rider() ->
  Rider = #{id => rl_util:id(), name => <<"Bob">>},
  {StateName1, _State1} = rl_race:register_rider(?RACE, Rider),

  {StateName1, State2} = rl_race:get_state(?RACE),

  % now there's 1 Rider
  ?assertEqual(registration, StateName1),
  ?assertEqual(1, maps:get(rider_count, State2)),
  ?assertEqual([Rider], maps:get(riders, State2)).

min_rider_count_satisfied_so_race_not_cancelled() ->
  {StateName, State} = rl_race:get_state(?RACE),
  ?assertEqual(registration, StateName),
  ?assertEqual(
    true,
    maps:get(rider_count, State) >= maps:get(min_rider_count, State)
  ),

  %% TODO: might be a better way than sleeping for the registration time to confirm check
  timer:sleep(maps:get(registration_time, State) * 1000),

  {StateName, State} = rl_race:get_state(?RACE),
  ?assertEqual(registration, StateName).

max_rider_count_reached_so_transition_to_registration_full() ->
  {StateName, State} = rl_race:get_state(?RACE),
  ?assertEqual(registration, StateName),
  ?assertEqual(1, maps:get(rider_count, State)),
  ?assertEqual(2, maps:get(max_rider_count, State)),

  Rider = #{id => rl_util:id(), name => <<"Jerry">>},
  {StateName1, _State1} = rl_race:register_rider(?RACE, Rider),

  {StateName1, State1} = rl_race:get_state(?RACE),
  ?assertEqual(registration_full, StateName1),
  ?assertEqual(2, maps:get(rider_count, State1)),
  ?assertEqual(2, maps:get(max_rider_count, State1)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
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