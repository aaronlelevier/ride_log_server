%%==============================================================================
%% Base Protocol
%%==============================================================================

-define(APP, ride_log).

%%------------------------------------------------------------------------------
%% Latitude
%%------------------------------------------------------------------------------
-type lat() :: float().
%%------------------------------------------------------------------------------
%% Longitude
%%------------------------------------------------------------------------------
-type lng() :: float().
%%------------------------------------------------------------------------------
%% Lat/Lng Position
%%------------------------------------------------------------------------------
-type position() :: {lat(), lng()}.
%%------------------------------------------------------------------------------
%% Id - unique id
%%------------------------------------------------------------------------------
-type id() :: binary().
%%------------------------------------------------------------------------------
%% Race - unique id of a race
%%------------------------------------------------------------------------------
-type race() :: id().
%%------------------------------------------------------------------------------
%% Point Name - describes the Point
%% Default:
%% - undefined
%% Reserved point names are:
%% - start
%% - finish
%%------------------------------------------------------------------------------
-type point_name() :: atom().
%%------------------------------------------------------------------------------
%% Point - all data that makes up a single Point on a Ride
%%------------------------------------------------------------------------------
-type point() :: #{id := id(),
                   lat := lat(),
                   lng := lng(),
                   time := erlang:timestamp(),
                   elevation := integer(),
                   name => point_name()}.

%%------------------------------------------------------------------------------
%% Ride
%%------------------------------------------------------------------------------
-record(ride, {id :: id(), name :: binary()}).

-type ride() :: #ride{}.

%%------------------------------------------------------------------------------
%% Rider
%%------------------------------------------------------------------------------
-record(rider, {id :: id(), name :: binary()}).

-type rider() :: #rider{}.

%%------------------------------------------------------------------------------
%% Ride Point
%%------------------------------------------------------------------------------
-record(ride_point,
        {id :: id(),
         ride :: ride(),
         rider :: rider(),
         point_dt :: erlang:timestamp(),
         point_name :: atom(),
         point_lat :: float(),
         point_lng :: float()}).

-type ride_point() :: #{id => id(),
                        ride => ride(),
                        rider => rider(),
                        point_dt => erlang:timestamp(),
                        point_name => atom(),
                        point_lat => float(),
                        point_lng => float()}.
%%------------------------------------------------------------------------------
%% Seconds
%%------------------------------------------------------------------------------
-type seconds() :: integer().
%%------------------------------------------------------------------------------
%% Points
%% @doc Should be a list of a minimum of 2 Points where the first and last Point
%% in the list are the Start and End Points
%%------------------------------------------------------------------------------
-type points() :: [point()].
%%------------------------------------------------------------------------------
%% Race State Name
%%------------------------------------------------------------------------------
-type race_state_name() :: registration |
                           registration_full |
                           cancelled |
                           prepare_for_start |
                           start |
                           finished.

%%------------------------------------------------------------------------------
%% Race State
%% @doc The race state, of which 3 fields are computed:
%% - id: the name of the race
%% - state_name: the current state name of the race
%% - cancellation_check_ref: Erlang ref() to the cancellation policy of the race
%%------------------------------------------------------------------------------
-record(race_state,
        {id :: race(),
         state_name :: race_state_name(),
         cancellation_check_ref :: reference(),
         rider_count :: integer(),
         riders :: [rider()],
         min_rider_count :: seconds(),
         max_rider_count :: seconds(),
         registration_time :: seconds(),
         prepare_for_start_time :: seconds(),
         race_time :: seconds(),
         points :: points()}).

-type race_state() :: #{id => race(),
                        state_name => race_state_name(),
                        cancellation_check_ref => reference() | undefined,
                        rider_count => integer(),
                        riders => [rider()],
                        min_rider_count => seconds(),
                        max_rider_count => seconds(),
                        registration_time => seconds(),
                        prepare_for_start_time => seconds(),
                        race_time => seconds(),
                        points => points()}.
