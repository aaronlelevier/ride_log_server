%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 18. Oct 2020 9:09 AM
%%%-------------------------------------------------------------------
-module(rl_db_ride_SUITE).
-author("Aaron Lelevier").
-vsn(1.0).

%% CT
-export([
  all/0
]).

%% Tests
-export([
  insert_and_lookup/1
]).

%%%===================================================================
%%% API
%%%===================================================================
all() ->
  [
    insert_and_lookup
  ].

%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------
insert_and_lookup(_Config) ->
  ok = rl_db:start(),

  % ride info
  Id = rl_util:id(),
  ExpectedRide = #{id => Id, name => <<"test ride">>},

  % insert Ride
  ok = rl_db_ride:insert(ExpectedRide),

  % lookup Ride
  {ok, Rides} = rl_db_ride:lookup(Id),
  1 = length(Rides),
  [Ride|_] = Rides,
  Ride = ExpectedRide,

  ok = rl_db:stop().

