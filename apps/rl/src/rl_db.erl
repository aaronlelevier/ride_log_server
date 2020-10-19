%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Mnesia DB backend. Currently all Mnesia read/write operations
%%% live here
%%%
%%% @end
%%% Created : 12. Oct 2020 9:21 AM
%%%-------------------------------------------------------------------
-module(rl_db).
-author("Aaron Lelevier").
-vsn(1.0).
-export([
  start/0,
  stop/0,
  create_db/0,
  start_db/0,
  write/1,
  create_ride/1,
  create_rider/1,
  lookup/2,
  match/1,
  select/1
]).

%% Required for transactions
-include_lib("stdlib/include/qlc.hrl").

-include("ride_log.hrl").

-define(TIMEOUT, 2000).

%%%===================================================================
%%% API
%%%===================================================================

%% TODO: will need to add a "name" param for dependency injection and test suite
-spec start() -> ok.
start() ->
  ok = create_db(),
  ok = start_db(),
  ok.

-spec stop() -> ok | {error, any()}.
stop() ->
  application:stop(mnesia).

-spec create_db() -> ok.
create_db() ->
  case mnesia:create_schema([node()]) of
    {error, {_, {already_exists, _}}} ->
      lager:info("DB Schema already exists, skipping"),
      ok;
    ok ->
      lager:info("DB Schema created"),
      ok
  end.

-spec start_db() -> ok.
start_db() ->
  case application:start(mnesia) of
    {error, {already_started, mnesia}} ->
      lager:info("Mnesia already started, skipping"),
      ok;
    ok ->
      lager:info("Mnesia started"),
      ok
  end,
  load_tables(),
  wait_for_tables(),
  ok.


%%------------------------------------------------------------------------------
%% Write Queries
%%------------------------------------------------------------------------------
-spec write(tuple()) -> ok.
write(Record) when is_tuple(Record) ->
  mnesia:dirty_write(Record).

-spec create_ride(binary()) -> {ok, RideId} when
  RideId :: id().
create_ride(Name) ->
  RideId = rl_util:id(),
  %% TODO: might need to wrap in a transaction
  ok = mnesia:dirty_write({ride, RideId, Name}),
  {ok, RideId}.

-spec create_rider(binary()) -> {ok, RiderId} when
  RiderId :: id().
create_rider(Name) ->
  RiderId = rl_util:id(),
  %% TODO: might need to wrap in a transaction
  ok = mnesia:dirty_write({rider, RiderId, Name}),
  {ok, RiderId}.

%%------------------------------------------------------------------------------
%% Read Queries
%%------------------------------------------------------------------------------
-spec lookup(atom(), id()) -> {ok, [tuple()]}.
lookup(Table, Key) ->
  {ok, mnesia:dirty_read(Table, Key)}.

-spec match(tuple()) -> {ok, [tuple()]}.
match(Pattern) when is_tuple(Pattern) ->
  {ok, mnesia:dirty_match_object(Pattern)}.

%%------------------------------------------------------------------------------
%% Transactional Operations
%%------------------------------------------------------------------------------
select(Table) ->
  do(qlc:q([X || X <- mnesia:table(Table)])).

%%%===================================================================
%%% Internal functions
%%%===================================================================
load_tables() ->
  [load_table({Mod, Name}) || {Mod, Name} <- tables()].

load_table({Mod, Name}) ->
  case mnesia:create_table(Name, Mod:opts()) of
    {atomic, ok} ->
      lager:info("Table created:~p", [Name]),
      ok;
    {aborted, {already_exists, _}} ->
      lager:info("Table already exists:~p", [Name]),
      ok
  end.

wait_for_tables() ->
  ok = mnesia:wait_for_tables(table_names(), ?TIMEOUT),
  ok.

tables() ->
  [
    {rl_db_ride_point, ride_point},
    {rl_db_ride, ride},
    {rl_db_rider, rider}
  ].

table_names() ->
  [Name || {_Mod, Name} <- tables()].

%%------------------------------------------------------------------------------
%% Transactional Operations
%%------------------------------------------------------------------------------
do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.
