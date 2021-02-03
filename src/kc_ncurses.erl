%%%-------------------------------------------------------------------
%%% @author Carlo Tomasin
%%% @copyright (C) 2021
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(kc_ncurses).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-define(ARCHIVEPATH, "archive.protected").
-include_lib("cecho/include/cecho.hrl").

-record(kc_ncurses_state, {window, prompt, password_prompt}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  ok = application:start(cecho),
  ok = cecho:cbreak(),
  % disable echo to hide user password
  ok = cecho:noecho(),

  {MaxY, MaxX} = cecho:getmaxyx(),
  Win = cecho:newwin(MaxY -2, MaxX, 0, 0),
  WinP = cecho:newwin(2, MaxX, MaxY-2, 0),

  {ok, #kc_ncurses_state{
    window = Win,
    prompt = make_prompt(WinP, input),
    password_prompt = make_prompt(WinP, password)
  }}.

handle_call(_Request, _From, State = #kc_ncurses_state{}) ->
  {reply, ok, State}.

handle_cast(_Request, State = #kc_ncurses_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #kc_ncurses_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #kc_ncurses_state{}) ->
  ok.

code_change(_OldVsn, State = #kc_ncurses_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
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