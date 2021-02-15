%%%-------------------------------------------------------------------
%%% @author Carlo Tomasin
%%% @copyright (C) 2021
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(kc_ncurses).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([prompt_for_path_password/0, updated/1, edit/1, show/1, clean/0]).

-define(SERVER, ?MODULE).

-include_lib("cecho/include/cecho.hrl").
-include("kc.hrl").

-record(ncurses_state, {window, prompt, password_prompt}).

-define(ARCHIVEPATH_DEFAULT, "archive.protected").

%%%===================================================================
%%% Interface functions
%%%===================================================================

%% @doc Commands the GUI module to ask the user for the password to
%% unlock the password storage media
prompt_for_path_password() ->
  gen_server:cast(?SERVER, prompt_for_path_password).

updated(What = accounts) ->
  gen_server:cast(?SERVER, {updated, What}).

edit(Account) ->
  gen_server:cast(?SERVER, {edit, Account}).

show(Account) ->
  gen_server:cast(?SERVER, {show, Account}).

clean() ->
  gen_server:cast(?SERVER, clean).

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
  Win = cecho:newwin(MaxY -1, MaxX, 0, 0),
  WinP = cecho:newwin(1, MaxX, MaxY-1, 0),
  {ok, #ncurses_state{
    window = Win,
    prompt = make_prompt(WinP, input),
    password_prompt = make_prompt(WinP, password)
  }}.

handle_call(_Request, _From, State = #ncurses_state{}) ->
  {reply, ok, State}.

handle_cast(prompt_for_path_password, State = #ncurses_state{ prompt= Prompt, password_prompt=PPrompt }) ->
  Path = case Prompt(io_lib:format("Insert archive path [~s]", [?ARCHIVEPATH_DEFAULT])) of
           "" -> ?ARCHIVEPATH_DEFAULT;
           P -> P
         end,
  Pwd = PPrompt("Insert archive password"),
  kc_controller:user_path_password(Path,Pwd),
  {noreply, State};

handle_cast({updated, accounts}, State = #ncurses_state{window=W, prompt=Prompt }) ->
  A = kc_server:first(),
  cecho:werase(W),
  {ok, IdNext} = list_account(A, W, 0),
  Text = Prompt( io_lib:format("Select an account by keyword or by index (1-~B 0:add new [q]uit)", [IdNext]) ),
  kc_controller:command(get_command(Text)),
  {noreply, State};

handle_cast({edit, {account, M}}, State = #ncurses_state{ prompt=Prompt }) ->
  Acq = fun(Label, Field) ->
    Prev = maps:get(Field, M, ""),
    case Prompt(io_lib:format("~s [~s]",[Label, Prev])) of
      "" -> Prev;
      New -> New
    end
  end,
  Title = Acq("Title", title),
  Url = Acq("URL", url),
  Username = Acq("Username", username),
  Password = Acq("Password", password),
  Notes = Acq("Notes", notes),
  Other = Acq("Other", other),
  case Prompt("[s]ave or [C]ancel')?") of
    "s" ->
      Account1 = {account, M#{
        title => Title,
        url => Url,
        username => Username,
        password => Password,
        notes => Notes,
        other => Other
      }},
      kc_controller:command({save, Account1});
    _ ->
      kc_controller:command({filter, ""})
  end,
  {noreply, State};

handle_cast({show, notfound}, State = #ncurses_state{window=W, prompt=Prompt }) ->
  A = kc_server:first(),
  cecho:werase(W),
  {ok, IdNext} = list_account(A, W, 0),
  Text = Prompt( io_lib:format("[not found] Select an account by keyword or by index (1-~B 0:add new [q]uit)", [IdNext]) ),
  kc_controller:command(get_command(Text)),
  {noreply, State};

handle_cast({show, {account, M}}, State = #ncurses_state{ window = Win, prompt = Prompt }) ->
  AccountId = maps:get(id, M),
  Title = maps:get(title, M, ""),
  {MaxY, MaxX } = cecho:getmaxyx(Win),
  WinE = cecho:newwin(MaxY - 8, MaxX - 8, 4, 4),
  cecho:mvwaddstr(WinE, 0, 2, io_lib:format("Id: ~B", [AccountId])),
  cecho:mvwaddstr(WinE, 1, 2, io_lib:format("Title: ~s", [Title])),
  cecho:mvwaddstr(WinE, 2, 2, io_lib:format("URL: ~s", [maps:get(url, M, "")])),
  cecho:mvwaddstr(WinE, 3, 2, io_lib:format("Username: ~s", [maps:get(username, M, "")])),
  cecho:mvwaddstr(WinE, 4, 2, io_lib:format("Password: ~s", [maps:get(password, M, "")])),
  cecho:mvwaddstr(WinE, 5, 2, io_lib:format("Notes: ~s", [maps:get(notes, M, "")])),
  cecho:mvwaddstr(WinE, 6, 2, io_lib:format("Other: ~s", [maps:get(other, M, "")])),
  cecho:box(WinE, 0, 0),
  cecho:wrefresh(WinE),
  Cmd = case Prompt( "[e]dit, [d]elete] or [C]ancel?" ) of
          "e" ->
            {edit, AccountId};
          "d" ->
            case Prompt( io_lib:format("Confirm to delete \"~s\" ([y] or [N])?", [Title]) ) of
              "y" ->
                {delete, AccountId};
              _ ->
                {cancel, 0}
            end;
          _ ->
            {cancel, 0}
  end,
  kc_controller:command(Cmd),
  {noreply, State};

handle_cast(Request, State = #ncurses_state{}) ->
  ?LOG_DEBUG(#{ who => ?MODULE, what => "Unmanaged cast", data => Request, log => trace, level => error }),
  {noreply, State};

handle_cast(clean, State) ->
  application:stop(cecho),
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
  {filter, ""};
get_command(Text) when Text == "q" orelse Text == "Q" ->
  {quit, 0};
get_command(Text) when Text == "0" ->
  {addnew, 0};
get_command(Text) ->
  case string:to_integer(Text) of
    {Num, []} -> {select, Num};
    _ -> {filter, Text}
  end.
