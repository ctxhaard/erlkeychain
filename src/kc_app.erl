%%%-------------------------------------------------------------------
%% @doc kc public API
%% @end
%%%-------------------------------------------------------------------

-module(kc_app).

-include("kc.hrl").

-behaviour(application).

-export([start/0, start/2, stop/1]).

start(_StartType, StartArgs) ->
    kc_sup:start_link(StartArgs).

start() -> kc_app:start([], [server]).

stop(_State) ->
    ok.

%% internal functions
