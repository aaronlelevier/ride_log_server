%%%-------------------------------------------------------------------
%% @doc rl top level supervisor.
%% @end
%%%-------------------------------------------------------------------

%% TODO: This should be a top level 'sup' and 'rl_race' fsm has it's own 'sup'

-module(rl_sup).

-behaviour(supervisor).

-include("ride_log.hrl").

%% API
-export([start_link/0, start_race/2, stop_race/1]).

%% Callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

-spec start_race(race(), rl_race:state()) -> {ok, pid()}.
start_race(Race, Args) ->
    ChildId = rl_util:id(),
    ChildSpec = #{
        id => ChildId,
        start => {rl_race, start_link, [Race, Args]},
        restart => permanent,
        shutdown => 10, % Timeout
        type => worker,
        modules => [rl_race, gen_statem]
    },
    supervisor:start_child(?SERVER, ChildSpec).

%% @doc stops a race and removes it from the list of 'children'
-spec stop_race(race()) -> ok.
stop_race(Race) ->
    case get_child(Race) of
        {error, Race} ->
            ok;
        {ok, ChildId} ->
            ok = supervisor:terminate_child(?MODULE, ChildId),
            ok = supervisor:delete_child(?MODULE, ChildId),
            ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_child(Race) -> {ok, ChildId} | {error, Race} when
    ChildId :: race(),
    Race :: atom().
get_child(Race) ->
    case lists:keyfind(whereis(Race), 2, supervisor:which_children(?MODULE)) of
        false -> {error, Race};
        Tuple -> {ok, element(1, Tuple)}
    end.
