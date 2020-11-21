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
%% Race
%%------------------------------------------------------------------------------
-record(race_state,
        {id :: race(),
         state_name :: atom(),
         rider_count :: integer(),
         riders :: [rider()],
         min_rider_count :: seconds(),
         max_rider_count :: seconds(),
         registration_time :: seconds(),
         prepare_for_start_time :: seconds(),
         race_time :: seconds(),
         points :: points(),
         cancellation_check_ref :: reference()}).

-type race_state() :: #{id => race(),
                        state_name => atom() | undefined,
                        rider_count => integer(),
                        riders => [rider()],
                        min_rider_count => seconds(),
                        max_rider_count => seconds(),
                        registration_time => seconds(),
                        prepare_for_start_time => seconds(),
                        race_time => seconds(),
                        points => points(),
                        cancellation_check_ref => reference() | undefined}.
