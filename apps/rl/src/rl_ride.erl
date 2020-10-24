%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Ride server where a single Process maps to a single Ride,
%%% and Riders would then: join the ride, start the ride, and maybe finish
%%% @end
%%%-------------------------------------------------------------------
-module(rl_ride).

-behaviour(gen_server).

-include("ride_log.hrl").

%% API
-export([start_link/1, add_rider/2, list_riders/1, create_ride/1]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% Macros
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

-spec create_ride(binary()) -> {ok, rl_db_ride:item()}.
create_ride(Name) ->
    {ok, RideId} = rl_db:create_ride(Name),
    {ok, _Pid} = start_link(RideId),
    Ride = #{id => RideId, name => Name},
    {ok, Ride}.

add_rider(RideId, RiderId) ->
    gen_server:call(RideId, {add_rider, RiderId}).

list_riders(RideId) ->
    gen_server:call(RideId, list_riders).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

-spec start_link(id()) -> {ok, pid()}.
start_link(RideId) ->
    gen_server:start_link(?MODULE, [RideId], []).

-spec init(id()) -> {ok, map()}.
init(RideId) ->
    {ok, #{id => RideId, riders => []}}.

handle_call({add_rider, RiderId}, _From, State0) ->
    State = State0#{riders := [RiderId | maps:get(riders, State0)]},
    {reply, ok, State};
handle_call(list_riders, _From, State) ->
    Reply = maps:get(riders, State),
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
