%%%-------------------------------------------------------------------
%% @doc kc public API
%% @end
%%%-------------------------------------------------------------------

-module(kc_app).

-include("kc.hrl").

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    kc_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
