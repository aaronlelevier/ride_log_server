%%%-------------------------------------------------------------------
%% @doc rl public API
%% @end
%%%-------------------------------------------------------------------

-module(rl_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    rl_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
