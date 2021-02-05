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
-export([prompt_for_password/0, updated/1]).

-define(SERVER, ?MODULE).

-include_lib("cecho/include/cecho.hrl").
-include("kc.hrl").

-record(ncurses_state, {window, prompt, password_prompt}).

%%%===================================================================
%%% Interface functions
%%%===================================================================

%% @doc Commands the GUI module to ask the user for the password to
%% unlock the password storage media
prompt_for_password() ->
  gen_server:cast(?SERVER, prompt_for_password).

updated(What = accounts) ->
  gen_server:cast(?SERVER, {updated, What}).

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

  {ok, #ncurses_state{
    window = Win,
    prompt = make_prompt(WinP, input),
    password_prompt = make_prompt(WinP, password)
  }}.

handle_call(_Request, _From, State = #ncurses_state{}) ->
  {reply, ok, State}.

handle_cast(prompt_for_password, State = #ncurses_state{ password_prompt=Prompt }) ->
  Pwd = Prompt("Insert your password"),
  kc_client:user_password(Pwd),
  {noreply, State};
handle_cast({updated, accounts}, State = #ncurses_state{window=W, prompt=Prompt }) ->
  A = kc_server:first(),
  cecho:werase(W),
  {ok, IdNext} = list_account(A, W, 0),
  Text = Prompt( io_lib:format("Select an account by keyword or by index (1-~B 0:add new [q]uit)", [IdNext-1]) ),
  kc_client:command(get_command(Text)),
  {noreply, State};
handle_cast(Request, State = #ncurses_state{}) ->
  ?LOG_DEBUG(#{ who => ?MODULE, what => "Unmanaged cast", data => Request, log => trace, level => error }),
  {noreply, State}.

handle_info(_Info, State = #ncurses_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #ncurses_state{}) ->
  ok.

code_change(_OldVsn, State = #ncurses_state{}, _Extra) ->
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

list_account(notfound, W, N) ->
  cecho:wrefresh(W),
  { ok, N };
list_account(A, W, N) ->
  case cecho:getmaxyx(W) of
    {MaxY, _} when N > MaxY ->
      cecho:wrefresh(W),
      {ok, N};
    _ ->
      {account, #{ id := Id, title := Title }} = A,
      cecho:mvwaddstr(W, N, 0, io_lib:format("~B: ~s", [Id, Title])),
      list_account(kc_server:next(), W, N + 1)
  end.

get_command([]) ->
  all;
get_command(Text) when Text == "q" orelse Text == "Q" ->
  quit;
get_command(Text) when Text == "0" ->
  addnew;
get_command(Text) ->
  case string:to_integer(Text) of
    {Num, []} -> Num;
    _ -> Text
  end.