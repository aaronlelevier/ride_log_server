%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc 'ride' table spec
%%%
%%% @end
%%% Created : 12. Oct 2020 9:30 AM
%%%-------------------------------------------------------------------
-module(rl_db_ride).
-author("Aaron Lelevier").
-vsn(1.0).
-include("ride_log.hrl").

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------
-export([
  insert/1,
  lookup/1,
  from_item/1,
  to_item/1
]).

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------
-type item() :: #{
id => id(),
name => binary()
}.

-export_type([
  item/0
]).

%%------------------------------------------------------------------------------
%% Behavior rl_db_table
%%------------------------------------------------------------------------------
-behaviour(rl_db_table).
-export([name/0, opts/0]).

name() -> ride.

opts() -> [
  {attributes, record_info(fields, ride)},
  {disc_copies, [node()]},
  {type, bag},
  {storage_properties, [{ets, []}]}
].

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

-spec from_item(map()) -> ride().
from_item(#{
  id := Id,
  name := Name
}) ->
  #ride{
    id = Id,
    name = Name
  }.

-spec to_item(ride()) -> map().
to_item(#ride{
  id = Id,
  name = Name
}) -> #{
  id => Id,
  name => Name}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
