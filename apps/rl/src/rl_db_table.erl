%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Callback module for a Mnesia table
%%% Notes:
%%% - how to resolve a custom behavior: https://github.com/ignatov/intellij-erlang/issues/762
%%%
%%% @end
%%% Created : 12. Oct 2020 9:30 AM
%%%-------------------------------------------------------------------
-module(rl_db_table).

-author("Aaron Lelevier").

-vsn(1.0).

%%------------------------------------------------------------------------------
%% Callbacks
%%------------------------------------------------------------------------------
-callback name() -> atom().
-callback opts() -> proplists:proplist().
