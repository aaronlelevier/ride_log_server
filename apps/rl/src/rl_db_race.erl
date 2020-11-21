%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 17. Nov 2020 6:20 AM
%%%-------------------------------------------------------------------
-module(rl_db_race).

-author("Aaron Lelevier").

-vsn(1.0).

-include("ride_log.hrl").

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------
-export([insert/1, lookup/1, from_item/1, to_item/1]).

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------
-type item() :: race_state().

-export_type([item/0]).

%%------------------------------------------------------------------------------
%% Behavior rl_db_table
%%------------------------------------------------------------------------------
-behaviour(rl_db_table).

-export([name/0, opts/0]).

name() ->
    race.

opts() ->
    [{attributes, record_info(fields, race_state)},
     {disc_copies, [node()]},
     {type, bag},
     {storage_properties, [{ets, []}]}].

%%%===================================================================
%%% API
%%%===================================================================

-spec insert(item()) -> ok | {error, any()}.
insert(Map) when is_map(Map) ->
    Record = from_item(Map),
    rl_db:write(Record).

-spec lookup(id()) -> {ok, [item()]}.
lookup(Id) ->
    {ok, Items} = rl_db:lookup(name(), Id),
    {ok, [to_item(Item) || Item <- Items]}.

-spec from_item(item()) -> #race_state{}.
from_item(#{id := Race,
            state_name := StateName,
            rider_count := RiderCount,
            riders := Riders,
            min_rider_count := MinRiderCount,
            max_rider_count := MaxRiderCount,
            registration_time := RegistrationTime,
            prepare_for_start_time := PrepareForStartTime,
            race_time := RaceTime,
            points := Points,
            cancellation_check_ref := CancellationCheckRef}) ->
    #race_state{id = Race,
                state_name = StateName,
                rider_count = RiderCount,
                riders = [rl_db_rider:from_item(X) || X <- Riders],
                min_rider_count = MinRiderCount,
                max_rider_count = MaxRiderCount,
                registration_time = RegistrationTime,
                prepare_for_start_time = PrepareForStartTime,
                race_time = RaceTime,
                points = Points,
                cancellation_check_ref = CancellationCheckRef}.

-spec to_item(#race_state{}) -> item().
to_item(#race_state{id = Race,
                    state_name = StateName,
                    rider_count = RiderCount,
                    riders = Riders,
                    min_rider_count = MinRiderCount,
                    max_rider_count = MaxRiderCount,
                    registration_time = RegistrationTime,
                    prepare_for_start_time = PrepareForStartTime,
                    race_time = RaceTime,
                    points = Points,
                    cancellation_check_ref = CancellationCheckRef}) ->
    #{id => Race,
      state_name => StateName,
      rider_count => RiderCount,
      riders => [rl_db_rider:to_item(X) || X <- Riders],
      min_rider_count => MinRiderCount,
      max_rider_count => MaxRiderCount,
      registration_time => RegistrationTime,
      prepare_for_start_time => PrepareForStartTime,
      race_time => RaceTime,
      points => Points,
      cancellation_check_ref => CancellationCheckRef}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
