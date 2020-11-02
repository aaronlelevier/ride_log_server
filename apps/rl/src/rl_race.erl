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
-export([registration/3, registration_full/3, cancelled/3]).
%% gen_statem callbacks
-export([init/1, format_status/2, terminate/3, code_change/4, callback_mode/0]).

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

-type state_name() :: registration |
                      registration_full |
                      cancelled |
                      prepare_for_start |
                      start |
                      finished.
-type seconds() :: integer().
%% @doc Should be a list of a minimum of 2 Points where the first and last Point
%% in the list are the Start and End Points
-type points() :: [point()].
-type state() :: #{rider_count => integer(),
                   riders => [rider()],
                   min_rider_count => seconds(),
                   max_rider_count => seconds(),
                   registration_time => seconds(),
                   prepare_for_start_time => seconds(),
                   race_time => seconds(),
                   points => points(),
                   cancellation_check_ref => reference() | undefined}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
-spec start_link(atom(), state()) -> {ok, pid()}.
start_link(Name, Args) ->
    gen_statem:start_link({local, Name}, ?MODULE, Args, []).

stop(Name) ->
    gen_statem:stop(Name).

-spec get_state(atom()) -> {state_name(), state()}.
get_state(Name) ->
    gen_statem:call(Name, get_state).

cancel(Name) ->
    gen_statem:call(Name, cancel).

register_rider(Name, Rider) ->
    gen_statem:call(Name, {register_rider, Rider}).

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
           {next_state, registration_full, NewState, [{reply, From, {registration_full, State}}]};
       true ->
           {keep_state, NewState, [{reply, From, {registration, State}}]}
    end;
registration({call, From}, get_state, State) ->
    {keep_state, State, [{reply, From, {registration, State}}]};
registration(EventType, EventContent, State) ->
    handle_event(EventType, EventContent, State).

registration_full({call, From}, get_state, State) ->
    {keep_state, State, [{reply, From, {registration_full, State}}]};
registration_full(EventType, EventContent, State) ->
    handle_event(EventType, EventContent, State).

cancelled({call, From}, get_state, State) ->
    {keep_state, State, [{reply, From, {cancelled, State}}]};
cancelled(EventType, EventContent, State) ->
    handle_event(EventType, EventContent, State).

handle_event({call, From}, cancel, State) ->
    lager:debug("~p", [{{call, From}, cancel, State}]),
    {next_state, cancelled, State, [{reply, From, {cancelled, State}}]};
handle_event({call, _From},
             {cancellation_check, Ref},
             #{cancellation_check_ref := Ref} = State) ->
    lager:debug("~p", [{{call, _From}, {cancellation_check, Ref}, State}]),
    case maps:get(rider_count, State) < maps:get(min_rider_count, State) of
        true ->
            {next_state, cancelled, State};
        false ->
            {keep_state, State}
    end.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
-spec init(state()) -> {ok, registration, state()}.
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

-spec set_cancellation_check(state()) -> {ok, state()}.
set_cancellation_check(State) ->
    {registered_name, Name} = process_info(self(), registered_name),
    Ref = make_ref(),
    {ok, _TRef} =
        timer:apply_after(maps:get(registration_time, State) * 1000,
                          gen_statem,
                          call,
                          [Name, {cancellation_check, Ref}]),
    {ok, State#{cancellation_check_ref => Ref}}.
