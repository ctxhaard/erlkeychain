-module(kc_client).
-behavior(gen_statem).

-export([start_link/0]).
%% gen_statem callback functions
-export([init/1, callback_mode/0, terminate/3]).
%% interface functions
-export([user_password/1, command/1, loaded_event/0]).
%% state machine functions
-export([state_unloaded/3, state_loaded/3]).

-include("kc.hrl").

-define(NAME, ?MODULE).

-define(ARCHIVEPATH, "archive.protected").

%              +-----------------+
%              |    unloaded     |
%              +-------+---------+
%                      |
%               load   |    +-----+
%                      v    v     | query
%              +-------+----+-+---+
% select +-----+    loaded    |
%        |     +----------+---+ <-----+ save
%        v                |           |
%  +-----+--------+       |    +------+-------+
%  | account_sele |       +--->+ account_edit |
%  +----------+---+     new    +-------+------+
%             |                        ^
%             +------------------------+
%                      edit
%%====================================================================
%% interface functions
%%====================================================================
user_password(Pwd) ->
  gen_statem:cast(?NAME, {password, Pwd}).

command(Command) ->
  gen_statem:cast(?NAME,  {command, Command}).

loaded_event() ->
  gen_statem:cast(?NAME, loaded).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  ?LOG_DEBUG(#{ who => ?MODULE, what => ":start_link", log => trace, level => debug }),
  gen_statem:start_link({local, ?NAME}, ?MODULE, [], []).

init(_Args) ->
  kc_ncurses:prompt_for_password(),
  {ok, state_unloaded, []}.

callback_mode() ->
  [state_functions].

terminate(_Reason, _State, _Data) ->
  ok.

state_unloaded(cast, {password, Pwd}, Data) ->
  ?LOG_DEBUG(#{ who => ?MODULE, what => "state_unloaded received a password", log => trace, level => debug }),
  kc_server:load(?ARCHIVEPATH, Pwd),
  {keep_state, Data};
state_unloaded(cast, loaded, Data) ->
  ?LOG_DEBUG(#{ who => ?MODULE, what => "state_unloaded a loaded event", log => trace, level => debug }),
  {next_state, state_loaded, Data}.

state_loaded(EventType, EventContent, Data) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
