%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Rider server where a single Process maps to a single Rider
%%% @end
%%%-------------------------------------------------------------------
-module(rl_rider).

-behaviour(gen_server).

-include("ride_log.hrl").

%% API
-export([create_rider/1, start_link/1, join_ride/2, is_riding/1, ride_info/1]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% Macros
-define(SERVER, ?MODULE).
-define(INIT_STATE, #{ride => undefined}).

%%%===================================================================
%%% API
%%%===================================================================

%% TODO: might want to support 'Name' arg as a 'string' and cast to 'binary'
%% TODO: might want to rename func to 'create' to be more polymorphic
-spec create_rider(binary()) -> {ok, rl_db_rider:item()}.
create_rider(Name) ->
    {ok, RiderId} = rl_db:create_rider(Name),
    {ok, _Pid} = start_link(RiderId),
    Rider = #{id => RiderId, name => Name},
    {ok, Rider}.

%% TODO: should not be able to join the same ride more than once
%% TODO: should not be able to join more than one ride at a time
%% @doc Rider begins a Ride
%% this should POST the Rider's first "start point" entry for the Ride
-spec join_ride(pid(), pid()) -> ok.
join_ride(RiderId, RideId) ->
    lager:debug("join_ride RiderId:~p", [RiderId]),
    gen_server:call(RiderId, {join_ride, RideId}).

is_riding(RiderId) ->
    gen_server:call(RiderId, is_riding).

ride_info(RiderId) ->
    gen_server:call(RiderId, ride_info).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

-spec start_link(id()) -> {ok, pid()}.
start_link(RiderId) ->
    gen_server:start_link(?MODULE, [RiderId], []).

-spec init(id()) -> {ok, map()}.
init(RiderId) ->
    {ok, #{id => RiderId, ride => undefined}}.

handle_call({join_ride, RideId}, _From, State0) ->
    lager:debug("handle_call join_ride RiderId:~p", [self()]),
    ok = rl_ride:add_rider(RideId, self()),
    State = State0#{ride := RideId},
    {reply, ok, State};
handle_call(is_riding, _From, State) ->
    Reply = maps:get(ride, State) =/= undefined,
    {reply, Reply, State};
handle_call(ride_info, _From, State) ->
    Reply = maps:get(ride, State),
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
