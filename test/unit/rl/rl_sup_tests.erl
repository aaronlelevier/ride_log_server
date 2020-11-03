%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 03. Nov 2020 6:05 AM
%%%-------------------------------------------------------------------
-module(rl_sup_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").


can_start_and_stop_race_test() ->
    {ok, _SupPid} = rl_sup:start_link(),
    ?assert(is_pid(whereis(rl_sup))),

    ?assertEqual([], supervisor:which_children(rl_sup)),

    % can start a race and child is added to supervisor
    Race = race11,
    {ok, _ChildPid} = rl_sup:start_race(Race, args()),

    Children = supervisor:which_children(rl_sup),
    ?assertEqual(1, length(Children)),

    % can stop a race, and children cleaned up on supervisor
    ?assertEqual(ok, rl_sup:stop_race(Race)),
    ?assertEqual([], supervisor:which_children(rl_sup)),

    % trying to stop a race that has already been stopped has no effect
    ?assertEqual(ok, rl_sup:stop_race(Race)),
    ?assertEqual([], supervisor:which_children(rl_sup)).


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