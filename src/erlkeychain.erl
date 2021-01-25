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
            if
                N =/= 0 -> ok;
                true    -> cecho:werase(Win)
            end,
            [A|Tail] = Accounts,
            {account, #{ id := Id, title := Title }} = A,
            cecho:mvwaddstr(Win, N, 0, io_lib:format("~B: ~s", [Id, Title])),
            accounts_summary(Win, N+1, Tail)
    end.

account_detail(A, Win) ->
    {MaxY, MaxX } = cecho:getmaxyx(Win),
    WinE = cecho:newwin(MaxY - 8, MaxX - 8, 4, 4),
    {account, M} = A,
    cecho:mvwaddstr(WinE, 0, 2, io_lib:format("Id: ~B", [maps:get(id, M)])),
    cecho:mvwaddstr(WinE, 1, 2, io_lib:format("Title: ~s", [maps:get(title, M, "")])),
    cecho:mvwaddstr(WinE, 2, 2, io_lib:format("URL: ~s", [maps:get(url, M, "")])),
    cecho:mvwaddstr(WinE, 3, 2, io_lib:format("Username: ~s", [maps:get(username, M, "")])),
    cecho:mvwaddstr(WinE, 4, 2, io_lib:format("Password: ~s", [maps:get(password, M, "")])),
    cecho:mvwaddstr(WinE, 5, 2, io_lib:format("Notes: ~s", [maps:get(notes, M, "")])),
    cecho:mvwaddstr(WinE, 6, 2, io_lib:format("Other: ~s", [maps:get(other, M, "")])),
    cecho:box(WinE, 0, 0),
    cecho:wrefresh(WinE).


loop(all, Accounts, Win, Prompt, Pwd) ->
    accounts_summary(Win, 0, Accounts),
    Text = Prompt( io_lib:format("Select an account by keyword or by index (1-~B 0:add new [q]uit)", [length(Accounts)]) ),
    loop(getcommand(Text), Accounts, Win, Prompt, Pwd);
loop(quit, _, _, _, _) ->
    ok;
loop(addnew, Accounts, Win, Prompt, Pwd) ->
    ok;
loop(Id, Accounts, Win, Prompt, Pwd) when is_integer(Id) ->
    [A|_] = [{account, Map} || {account, Map} <- Accounts, maps:get(id, Map) =:= Id],
    account_detail(A,Win),
    Text = Prompt( "[e]dit, [d]elete] or [C]ancel?" ),
    account_op(get_account_command(Text), A, Accounts, Win, Prompt, Pwd);
loop(Regex, Accounts, Win, Prompt, Pwd) ->
    FilteredAccounts =  case re:compile(Regex, [caseless]) of
                            {ok, MP} -> [ X || X <- Accounts, erlkc_account:matches(X, MP) ];
                            _ -> Accounts
                        end,
    accounts_summary(Win, 0, FilteredAccounts),
    Text = Prompt( io_lib:format("Select an account by keyword or by index (1-~B 0:add new [q]uit)", [length(Accounts)]) ),
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

get_account_command("e") -> edit;
get_account_command("d") -> delete;
get_account_command(_) -> cancel.

account_op(edit, A, Accounts, Win, Prompt, Pwd) ->
    ok;
account_op(delete, A, Accounts, Win, Prompt, Pwd) ->
    {account, M} = A,
    Text = Prompt( io_lib:format("Confirm to delete ~s ([y] or [N])?", [maps:get(title, M, "<no title>")])),
    case Text of
        "y" -> 
            Accounts1 = Accounts -- [A],
            erlkc_account:save(?ARCHIVEPATH, Pwd, Accounts1),
            loop(all, Accounts1, Win, Prompt, Pwd);
        _ -> 
            loop(all, Accounts, Win, Prompt, Pwd)
    end;
account_op(_, _, Accounts, Win, Prompt, Pwd) ->
    loop(all, Accounts, Win, Prompt, Pwd).
