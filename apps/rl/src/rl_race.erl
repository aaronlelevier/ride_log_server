%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 27. Oct 2020 6:11 AM
%%%-------------------------------------------------------------------
-module(rl_race).

-author("Aaron Lelevier").

-behaviour(gen_statem).

-include("ride_log.hrl").

%% API
-export([start_link/2, get_state/1, cancel/1, register_rider/2, stop/1]).
%% State callbacks
-export([registration/3, registration_full/3, cancelled/3, prepare_for_start/3]).
%% gen_statem callbacks
-export([init/1, format_status/2, terminate/3, code_change/4, callback_mode/0]).

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% Default 'state_name' to registration is for new FSM case, otherwise we
%% would restore to a previous 'state_name' when re-initializing the FSM
-spec start_link(race(), race_state()) -> {ok, pid()}.
start_link(Race, Args0) ->
    Args =
        Args0#{id => rl_util:id(),
               name => Race,
               state_name => maps:get(state_name, Args0, registration)},
    gen_statem:start_link({local, Race}, ?MODULE, Args, []).

-spec stop(race()) -> ok.
stop(Race) ->
    gen_statem:stop(Race).

-spec get_state(race()) -> {race_state_name(), race_state()}.
get_state(Race) ->
    gen_statem:call(Race, get_state).

-spec cancel(race()) -> {race_state_name(), race_state()}.
cancel(Race) ->
    gen_statem:call(Race, cancel).

-spec register_rider(race(), rl_db_rider:item()) -> {race_state_name(), race_state()}.
register_rider(Race, Rider) ->
    gen_statem:call(Race, {register_rider, Rider}).

%%%===================================================================
%%% state callbacks
%%%===================================================================

%% @doc registration: registers riders for the race. There are 2 scenarios:
%% A - can register
%% B - registration full
registration({call, From}, {register_rider, Rider}, State) ->
    RiderCount = maps:get(rider_count, State),
    MaxRiderCount = maps:get(max_rider_count, State),
    NewRiderCount = RiderCount + 1,
    NewState =
        State#{rider_count := NewRiderCount, riders := [Rider | maps:get(riders, State)]},
    if NewRiderCount =:= MaxRiderCount ->
           next_state({next_state,
                       registration_full,
                       NewState,
                       [{reply, From, {registration_full, State}}]});
       true ->
           {keep_state, NewState, [{reply, From, {registration, NewState}}]}
    end;
registration(EventType, EventContent, State) ->
    handle_event(EventType, EventContent, State).

registration_full(EventType, EventContent, State) ->
    handle_event(EventType, EventContent, State).

cancelled(EventType, EventContent, State) ->
    handle_event(EventType, EventContent, State).

prepare_for_start(EventType, EventContent, State) ->
    handle_event(EventType, EventContent, State).

handle_event({call, From}, get_state, State) ->
    StateName = maps:get(state_name, State),
    {keep_state, State, [{reply, From, {StateName, State}}]};
handle_event({call, From}, cancel, State) ->
    lager:debug("~p", [{{call, From}, cancel, State}]),
    next_state({next_state, cancelled, State, [{reply, From, ok}]});
handle_event({call, _From},
             {cancellation_check, Ref},
             #{cancellation_check_ref := Ref} = State) ->
    lager:debug("~p", [{{call, _From}, {cancellation_check, Ref}, State}]),
    case maps:get(rider_count, State) < maps:get(min_rider_count, State) of
        true ->
            next_state({next_state, cancelled, State});
        false ->
            next_state({next_state, prepare_for_start, State})
    end.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
-spec init(race_state()) -> {ok, registration, race_state()}.
init(State) ->
    lager:debug("~p", [State]),
    {ok, State2} = set_cancellation_check(State),
    {ok, registration, State2}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
-spec callback_mode() -> handle_event_function | state_functions.
callback_mode() ->
    state_functions.

%% TODO: not sure if this is needed to rebuild fsm if it terminates abnormally
%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
    Status = some_term,
    Status.

%% @private
%% @doc This function is called by a gen_statem when it is about to terminate
terminate(normal, StateName, State) ->
    lager:debug("terminate: ~p", [{normal, StateName, State}]),
    ok;
terminate(Reason, StateName, State) ->
    lager:error("terminate: ~p", [{Reason, StateName, State}]),
    ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec set_cancellation_check(race_state()) -> {ok, race_state()}.
set_cancellation_check(State) ->
    {registered_name, Name} = process_info(self(), registered_name),
    Ref = make_ref(),
    {ok, _TRef} =
        timer:apply_after(maps:get(registration_time, State) * 1000,
                          gen_statem,
                          call,
                          [Name, {cancellation_check, Ref}]),
    {ok, State#{cancellation_check_ref => Ref}}.

%% @doc helper for keeping the 'state_name' updated in the 'state'
-spec next_state(tuple()) -> tuple().
next_state({next_state, StateName, State0}) ->
    State = State0#{state_name := StateName},
    {next_state, StateName, State};
next_state({next_state, StateName, State0, Reply}) ->
    State = State0#{state_name := StateName},
    {next_state, StateName, State, Reply}.
