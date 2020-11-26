%%%-------------------------------------------------------------------
%% @doc api top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(api_handler).

-export([init/2]).

init(Req0, State) ->
    Req =
        cowboy_req:reply(200,
                         #{<<"content-type">> => <<"text/plain">>},
                         <<"Hello Erlang!">>,
                         Req0),
    {ok, Req, State}.
