%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 21. Nov 2020 8:52 AM
%%%-------------------------------------------------------------------
-module(rl_db_race_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").

to_and_from_item_test() ->
    Race = race_nov_21,
    {ok, _PId} = rl_race:start_link(Race, rl_test:args()),

    {_StateName, State} = rl_race:get_state(Race),

    Ret = rl_db_race:from_item(State),
    ?assert(is_tuple(Ret)),

    Ret2 = rl_db_race:to_item(Ret),
    ?assertEqual(State, Ret2).