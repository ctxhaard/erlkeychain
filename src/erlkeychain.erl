-module(erlkeychain).
-author('c.tomasin@gmail.com').

%% API exports
-export([main/1]).

-define(ARCHIVEPATH, "archive.protected").

-import_lib("cecho/include/cecho.erl").
%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_Args) ->
    ok = application:start(cecho),
    ok = cecho:cbreak(),
    ok = cecho:noecho(),

    {MaxY, MaxX} = cecho:getmaxyx(),
    Win = cecho:newwin(MaxY, MaxX, MaxY - 2, 0),

    cecho:waddstr(Win,"Insert your password: "),
    cecho:wrefresh(Win),
    %cecho:waddstr(Win, "Your password is: "),
    %cecho:waddstr(Win, Pwd),
    %cecho:wrefresh(Win),
    Accounts = erlkc_account:load(?ARCHIVEPATH, get_password()),
    accounts_summary(Accounts),

    % TEMP: to avoid program to shut down immediately
    cecho:getch(),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
get_password() ->
    get_password([]).

get_password(Tail) ->
    case cecho:getch() of
        $\n -> 
            lists:reverse(Tail);
        Char -> 
            get_password([Char|Tail])
    end.

accounts_summary(_Accounts) ->
    % TODO: print accounts summary
    ok.