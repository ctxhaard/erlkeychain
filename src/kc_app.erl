%%%-------------------------------------------------------------------
%% @doc kc public API
%% @end
%%%-------------------------------------------------------------------

-module(kc_app).

-include("kc.hrl").

-behaviour(application).

-export([start/0, start/2, stop/1]).

%% @type server() = atom()
%% @type client() = atom()
%% @spec start(StartType :: term(), StartArgs :: [ server() | client() ]) -> pid()
start(_StartType, StartArgs) ->
    kc_sup:start_link(StartArgs).

start() -> kc_app:start([], [server, client]).

stop(_State) ->
    ok.

%% internal functions
