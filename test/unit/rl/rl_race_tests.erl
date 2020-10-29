%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 29. Oct 2020 6:44 AM
%%%-------------------------------------------------------------------
-module(rl_race_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").

a_race_can_be_started_and_cancelled_test() ->
  Race = race0,
  Args = #{
    rider_count => 2,
    registration_time => 2,
    prepare_for_start_time => 2,
    race_time => 10,
    points => 3
  },
  {ok, _Pid} = rl_race:start_link(Race, Args),

  % initial state
  {StateName1, State1} = rl_race:get_state(Race),

  ?assertEqual(registration, StateName1),
  ?assertEqual(Args, State1),

  % cancel race
  {StateName2, State2} = rl_race:cancel(Race),

  ?assertEqual(cancelled, StateName2),
  ?assertEqual(Args, State2).
