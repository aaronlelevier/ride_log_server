%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Example functions that a RideLog client might call
%%%
%%% @end
%%% Created : 10. Oct 2020 10:06 AM
%%%-------------------------------------------------------------------
-module(rl_client).

-author("Aaron Lelevier").

-vsn(1.0).

-include("ride_log.hrl").

-export([start/0]).

%% @doc A Rider starts a new Ride
start() ->
    % start mnesia
    ok = rl_db:start(),

    % new ride
    {ok, Ride} = rl_ride:create_ride(<<"My first ride">>),

    % new rider
    {ok, Rider} = rl_rider:create_rider(<<"Aaron">>),

    %%  lager:debug("riders: ~p", [rl_ride:list_riders(RideId)]),
    %%  lager:debug("is_riding: ~p", [rl_rider:is_riding(RiderId)]),
    %%  lager:debug("ride_info: ~p", [rl_rider:ride_info(RiderId)]),
    %%
    %%  % Rider joins a Ride - blocking - must wait until Ride confirms
    %%  ok = rl_rider:join_ride(RiderId, RideId),
    %%
    %%  lager:debug("riders: ~p", [rl_ride:list_riders(RideId)]),
    %%  lager:debug("is_riding: ~p", [rl_rider:is_riding(RiderId)]),
    %%  lager:debug("ride_info: ~p", [rl_rider:ride_info(RiderId)]),
    {Ride, Rider}.
