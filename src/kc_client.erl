-module(kc_client).
-behavior(gen_statem).

-export([start_link/0]).
%% gen_statem callback functions
-export([init/1, callback_mode/0, terminate/3]).
%% state machine functions
-export([unloaded/3, list_accounts/3, show_account/3, edit_account/3]).

-include("kc.hrl").

-define(NAME, ?MODULE).

%              +-----------------+
%              |    unloaded     |
%              +-------+---------+
%                      |
%               load   |    +-----+
%                      v    v     | query
%              +-------+----+-+---+
% select +-----+ list_accounts|
%        |     +----------+---+ <-----+ save
%        v                |           |
%  +-----+--------+       |    +------+-------+
%  | show_account |       +--->+ edit_account |
%  +----------+---+     new    +-------+------+
%             |                        ^
%             +------------------------+
%                      edit
%%====================================================================
%% interface functions
%%====================================================================
start_link() ->
  ?LOG_DEBUG(#{ what => ":start_link", log => trace, level => debug }),
  gen_statem:start_link({local, ?NAME}, ?MODULE, [], []).

%%====================================================================
%% gen_statem callback module functions
%%====================================================================
init(Args) ->
  {ok, unloaded, []}.

callback_mode() ->
  [state_functions, state_enter].

terminate(Reason, State, Data) ->
  ok.

unloaded(enter, EventContent, Data) ->
  ?LOG_DEBUG(#{ what => "entering password_prompt", log => trace, level => debug }),
  {keep_state, Data};

unloaded(EventType, EventContent, Data) ->
  ?LOG_DEBUG(#{ what => "unloaded received an event", log => trace, level => debug }),
  {next_state, list_accounts, Data}.

list_accounts(EventType, EventContent, Data) ->
  {next_state, list_accounts, Data}.

show_account(EventType, EventContent, Data) ->
  {next_state, list_accounts, Data}.

edit_account(EventType, EventContent, Data) ->
  {next_state, list_accounts, Data}.

%%====================================================================
%% Internal functions
%%====================================================================
