%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 27. Oct 2020 6:11 AM
%%%-------------------------------------------------------------------
-module(rl_race).

-author("Aaron Lelevier").

-behaviour(gen_statem).

%% API
-export([start_link/2, get_state/1, cancel/1]).

%% State callbacks
-export([registration/3]).

%% gen_statem callbacks
-export([init/1, format_status/2, handle_event/4, terminate/3, code_change/4,
  callback_mode/0]).

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

-type state_name() :: registration | cancelled | prepare_for_start | start | finished.
-type seconds() :: integer().
-type point_lat() :: float().
-type point_lng() :: float().
-type point() :: {point_lat(), point_lng()}.
%% @doc Should be a list of a minimum of 2 Points where the first and last Point
%% in the list are the Start and End Points
-type points() :: [point()].
-type state() :: #{rider_count => integer(),
registration_time => seconds(),
prepare_for_start_time => seconds(),
race_time => seconds(),
points => points()}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
-spec start_link(atom(), state()) -> {ok, pid()}.
start_link(Name, Args) ->
  gen_statem:start_link({local, Name}, ?MODULE, Args, []).

-spec get_state(atom()) -> {state_name(), state()}.
get_state(Name) ->
  gen_statem:call(Name, get_state).

cancel(Name) ->
  gen_statem:call(Name, cancel).


%%%===================================================================
%%% state callbacks
%%%===================================================================

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
registration(EventType, EventContent, State) ->
  handle_event(EventType, EventContent, State).

handle_event({call, From}, get_state, State) ->
  {keep_state, State, [{reply, From, {registration, State}}]};
handle_event({call, From}, cancel, State) ->
  {next_state, cancelled, State, [{reply, From, {cancelled, State}}]}.


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
  - spec init(state()) -> {ok, registration, state()}.
init(State) ->
  lager:debug("~p", [State]),
  {ok, registration, State}.

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
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to terminate
terminate(_Reason, _StateName, _State) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
