-module(kc_client).
-behavior(gen_statem).

-export([start_link/0]).
%% gen_statem callback functions
-export([init/1, callback_mode/0, terminate/3]).
%% state machine functions
-export([password_prompt/3, list_accounts/3, show_account/3, edit_account/3]).

-include("kc.hrl").
-include_lib("cecho/include/cecho.hrl").

-define(ARCHIVEPATH, "archive.protected").

-define(NAME, ?MODULE).

%              +-----------------+
%              | password_prompt |
%              +-------+---------+
%                      |
%               load   |     +----+
%                      v     |    | query
%              +-------+-----v----+
% select +-----+ list_accounts|
%        |     +----------+---+ <-----+ save
%        v                |           |
%  +-----+--------+       |    +------+-------+
%  | show_account |       +--> | edit_account |
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
  ok = application:start(cecho),
  ok = cecho:cbreak(),

  {ok, password_prompt, []}.

callback_mode() ->
  [state_functions, state_enter].

terminate(Reason, State, Data) ->
  ok.

password_prompt(enter, EventContent, Data) ->
  ?LOG_DEBUG(#{ what => "entering password_prompt", log => trace, level => debug }),

  % disable echo to hide user password
  ok = cecho:noecho(),

  {MaxY, MaxX} = cecho:getmaxyx(),
  Win = cecho:newwin(MaxY -2, MaxX, 0, 0),
  WinP = cecho:newwin(2, MaxX, MaxY-2, 0),

  Prompt = make_prompt(WinP, input),
  Password = make_prompt(WinP, password),

  Pwd = Password("Insert your password"),

  %% @todo: how do I change state? change implementation here
  kc_account:load(?ARCHIVEPATH, Pwd),

  {keep_state, Data};

password_prompt(EventType, EventContent, Data) ->
  ?LOG_DEBUG(#{ what => "password_prompt received an event", log => trace, level => debug }),
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
make_prompt(Win, Type) ->
  fun(Text) ->
    cecho:werase(Win),
    cecho:mvwaddstr(Win, 0, 0,[Text, ": "]),
    cecho:wrefresh(Win),
    case Type of
      password -> getstr(password);
      _ -> getstr(Win)
    end
  end.

getstr(Win) ->
  getstr(Win,[]).

getstr(Win,Tail) ->
  case cecho:getch() of
    $\n ->
      lists:reverse(Tail);
    Char when Tail =:= []
      andalso (Char =:= $\b orelse Char =:= $\d) ->
      getstr(Win,Tail);
    Char when Char =:= $\b orelse Char =:= $\d ->
      [_|NewTail] = Tail,
      if Win =/= password ->
        {Y, X} = cecho:getyx(Win),
        cecho:mvwaddch(Win, Y, X -1, $\s),
        cecho:wrefresh(Win),
        cecho:wmove(Win, Y, X - 1);
        true -> ok
      end,
      getstr(Win,NewTail);
    Char ->
      if Win =/= password ->
        cecho:waddstr(Win, [Char]),
        cecho:wrefresh(Win);
        true -> ok
      end,
      getstr(Win, [Char|Tail])
  end.