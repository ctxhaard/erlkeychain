-module(kc_client).
-behavior(gen_statem).

-export([start_link/0]).
%% gen_statem callback functions
-export([init/1, callback_mode/0, terminate/3]).
%% interface functions
-export([user_path_password/2, command/1, loaded_event/0]).
%% state machine functions
-export([state_unloaded/3, state_loaded/3, state_selected/3, state_edit/3]).

-include("kc.hrl").

-define(NAME, ?MODULE).
-define(HANDLE_COMMON, ?FUNCTION_NAME(T, C, D) -> handle_common(T, C, D)).

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
%  |  selected    |       +--->+     edit     |
%  +----------+---+     new    +-------+------+
%             |                        ^
%             +------------------------+
%                      edit

%%====================================================================
%% interface functions (events)
%%====================================================================
user_path_password(Path, Pwd) ->
  gen_statem:cast(?NAME, {load, Path, Pwd}).

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
  kc_ncurses:prompt_for_path_password(),
  {ok, state_unloaded, []}.

callback_mode() ->
  [state_functions].

terminate(_Reason, _State, _Data) ->
  ok.

%% ------------------------------------------------
%%           UNLOADED STATE
%% ------------------------------------------------
state_unloaded(cast, {load, Path, Pwd}, Data) ->
%%  ?LOG_DEBUG(#{ who => ?MODULE, what => "state_unloaded received a password", log => trace, level => debug }),
  kc_server:load(Path, Pwd),
  {keep_state, Data};

state_unloaded(cast, loaded, Data) ->
%%  ?LOG_DEBUG(#{ who => ?MODULE, what => "state_unloaded a loaded event", log => trace, level => debug }),
  kc_ncurses:updated(accounts),
  {next_state, state_loaded, Data};
?HANDLE_COMMON.

%% ------------------------------------------------
%%           LOADED STATE
%% ------------------------------------------------
state_loaded(cast, {command, {addnew, _}}, Data) ->
  kc_ncurses:edit( kc_server:new_account() ),
  {next_state, state_edit, Data};

state_loaded(cast, {command, {select, AccountId}}, Data) ->
  A = kc_server:get(AccountId),
  kc_ncurses:show(A),
  case A of
    notfound -> {keep_state, Data};
    _ -> {next_state, state_selected, Data}
  end;

state_loaded(cast, {command, {filter, Pattern}}, Data) ->
  PatternNew = case Pattern of
    "" -> undefined;
    Other -> Other
  end,
  kc_server:set_pattern(PatternNew),
  kc_ncurses:updated(accounts),
  {keep_state, Data};
?HANDLE_COMMON.

%% ------------------------------------------------
%%           SELECTED STATE
%% ------------------------------------------------
state_selected(cast, {command, {delete, AccountId}}, Data) ->
  kc_server:delete(AccountId),
  {next_state, state_loaded, Data};

state_selected(cast, {command, {edit, AccountId}}, Data) ->
  A = kc_server:get(AccountId),
  kc_ncurses:edit(A),
  {next_state, state_edit, Data};

state_selected(cast, {command, {cancel, _}}, Data) ->
  kc_ncurses:updated(accounts),
  {next_state, state_loaded, Data};

state_selected(cast, loaded, Data) ->
  kc_ncurses:updated(accounts),
  {next_state, state_loaded, Data};
?HANDLE_COMMON.

%% ------------------------------------------------
%%           EDIT STATE
%% ------------------------------------------------
state_edit(cast, {command, {save, Account}}, Data) ->
  kc_server:put(Account),
  {next_state, state_loaded, Data};
?HANDLE_COMMON.

%% ------------------------------------------------
%%           COMMON EVENT HANDLERS
%% ------------------------------------------------
handle_common(cast, {command, {quit, _}}, Data) ->
  kc_ncurses:clean(),
  erlang:halt(0),
  {keep_state, Data};

handle_common(cast, loaded, Data) ->
  kc_ncurses:updated(accounts),
  {keep_state, Data}.

%%====================================================================
%% Internal functions
%%====================================================================
