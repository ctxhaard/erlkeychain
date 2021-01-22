-module(erlkeychain).
-author('c.tomasin@gmail.com').

%% API exports
-export([main/1]).

-define(ARCHIVEPATH, "archive.protected").

-include_lib("cecho/include/cecho.hrl").
%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_Args) ->
    ok = application:start(cecho),
    ok = cecho:cbreak(),
    % disable echo to hide user password
    ok = cecho:noecho(),

    {MaxY, MaxX} = cecho:getmaxyx(),
    Win = cecho:newwin(MaxY -2, MaxX, 0, 0),
    WinP = cecho:newwin(2, MaxX, MaxY-2, 0),

    Prompt = make_prompt(WinP, input),
    Password = make_prompt(WinP, password),

    Pwd = Password("Insert your password"),

    Accounts = erlkc_account:load(?ARCHIVEPATH, Pwd),

    loop(all, Accounts, Win, Prompt, Pwd),

    % TEMP: to avoid program to shut down immediately
    cecho:getch(),
    erlang:halt(0).

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
        Char ->
            if Win =/= password -> 
                cecho:waddstr(Win, [Char]),
                cecho:wrefresh(Win);
                true -> ok
            end,
            getstr(Win, [Char|Tail])
    end.

accounts_summary(Win, _N, []) ->
    cecho:wrefresh(Win);
accounts_summary(Win, N, Accounts) ->
    case cecho:getmaxyx(Win) of
        {MaxY, _} when N > MaxY ->
            cecho:wrefresh(Win);
        _ ->
            [A|Tail] = Accounts,
            {account, #{ title := Title }} = A,
            cecho:mvwaddstr(Win, N, 0, io_lib:format("~B: ~s", [N + 1, Title])),
            accounts_summary(Win, N+1, Tail)
    end.

loop(all, Accounts, Win, Prompt, Pwd) ->
    accounts_summary(Win, 0, Accounts),
    Text = Prompt( io_lib:format("Select an account by keyword or by index (1-~B 0:add new [Q]uit)", [length(Accounts)]) ),
    loop(getcommand(Text), Accounts, Win, Prompt, Pwd);
loop(quit, _, _, _, _) ->
    ok;
loop(addnew, Accounts, Win, Prompt, Pwd) ->
    ok;
loop(Command, Accounts, Win, Prompt, Pwd) when is_integer(Command) ->
    % stampare l'account selezionato
    ok;
loop(Regex, Accounts, Win, Prompt, Pwd) ->
    FilteredAccounts =  case re:compile(Regex, [caseless]) of
                            {ok, MP} -> [ X || X <- Accounts, erlkc_account:matches(X, MP) ];
                            _ -> Accounts
                        end,
    accounts_summary(Win, 0, FilteredAccounts),
    Text = Prompt( io_lib:format("Select an account by keyword or by index (1-~B 0:add new [Q]uit)", [length(Accounts)]) ),
    loop(getcommand(Text), Accounts, Win, Prompt, Pwd).

getcommand([]) ->
    all;
getcommand(Text) when Text == "q" orelse Text == "Q" ->
    quit;
getcommand(Text) when Text == "0" ->
    addnew;
getcommand(Text) ->
    case string:to_integer(Text) of
        {Num, []} -> Num;
        _ -> Text
    end.
