%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2020 9:47 AM
%%%-------------------------------------------------------------------
-module(rl_util).
-author("Aaron Lelevier").
-vsn(1.0).
-export([id/0]).
-include("ride_log.hrl").

-spec id() -> id().
id() ->
  list_to_binary(uuid:uuid_to_string(uuid:get_v4())).
