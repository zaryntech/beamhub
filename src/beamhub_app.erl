%%%-------------------------------------------------------------------
%% @doc beamhub public API
%% @end
%%%-------------------------------------------------------------------

-module(beamhub_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    beamhub_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
