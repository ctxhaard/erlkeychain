%%%-------------------------------------------------------------------
%% @doc erlkeychain public API
%% @end
%%%-------------------------------------------------------------------

-module(erlkeychain_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlkeychain_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
