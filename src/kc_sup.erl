%%%-------------------------------------------------------------------
%% @doc kc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(kc_sup).

-include("kc.hrl").

-behaviour(supervisor).

-export([start_link/0, start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, all).

start_link(StartArgs) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, StartArgs).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init(StartArgs) ->
  ChildObs = #{
      id => kc_observable,
      start => {kc_observable, start_link, []},
      restart => transient,
      type => worker
    },
  ChildModel = #{
      id => kc_model,
      start => {kc_server, start_link,[]},
      restart => transient,
      type => worker
    },
  ChildView = #{
      id => kc_view,
      start => {kc_ncurses, start_link, []},
      restart => transient,
      type => worker
    },
  ChildController = #{
      id => kc_controller,
      start => {kc_controller, start_link, []},
      restart => transient,
      type => worker
    },

  ChildSpecs = case StartArgs of
    [all] -> [
      ChildObs,
      ChildModel,
      ChildView,
      ChildController
    ];
    [] -> [
      ChildObs,
      ChildModel,
      ChildView,
      ChildController
    ];
    [server] -> [
      ChildObs,
      ChildModel
      ];
    [client] -> [
      ChildView,
      ChildController
      ]
  end,
  
  SupFlags = #{strategy => one_for_all,
    intensity => 0,
    period => 1},
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions
