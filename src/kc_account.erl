-module(kc_account).
-author('c.tomasin@gmail.com').

-include("kc.hrl").

-export([load/2, save/3, new/1, get_id/1, matches/2, main/1]).

%% ----------------------------------------------------------------------------
%% The public interface
%% ----------------------------------------------------------------------------

%% @type account() = {account, any()}

%% @spec load(FilePath :: iolist(), Pwd :: iolist()) -> [account()]
%% @doc Loads accounts from persistence file using password to decrypt it
load(FilePath, Pwd) ->
    process_flag(trap_exit, true),
    % openssl enc -d -aes-256-cbc -md sha256 -in <file>
    % passo la password nello standard input di openssl, in modo
    % che la password non sia visibile nella lista dei processi
    FilePathBin = list_to_binary(FilePath),
    Port = open_port(
        {spawn, <<"openssl enc -d -aes-256-cbc -md sha256 -in ", FilePathBin/binary>>},
        [ use_stdio, stderr_to_stdout, exit_status, {line, 255} ]
    ),
    decode(Port, Pwd).

%% @spec save(FileName ::iolist(), Pwd :: iolist(), Accounts :: [account()]) -> ok
%% @doc Save the whole accounts list to persistence file, using password to encrypt it.
%%
%% The current file is moved unchanged to a backup copy, prior to create the new one
save(FileName, Pwd, Accounts) ->
    file:copy(FileName, backup_name(FileName, calendar:universal_time())),
    FileNameBin = list_to_binary(FileName),
    Port = open_port(
        {spawn, <<"openssl enc -aes-256-cbc -md sha256 -salt -out ", FileNameBin/binary>>},
        [ use_stdio, stderr_to_stdout, exit_status, {line, 255} ]
    ),
    ?LOG_DEBUG(#{ what => Port, log => trace, level => debug }),
    encode(Port, Pwd, Accounts).

%% @spec new(Accounts :: [account()]) -> account()
%% @doc Creates a new, empty account. Accounts list is used to allocate a new, greater, id.
new(Accounts) ->
    MaxFun = fun({account, M}, Max) ->
        X = maps:get(id, M, 0),
        if X > Max -> X;
            true -> Max
        end
             end,
    MaxVal = lists:foldl(MaxFun, 0, Accounts),
    {account, #{ id => (MaxVal + 1), title => <<"">> }}.

%% @spec get_id(Account) -> integer()
%% @doc Gets the account identifier.
get_id({account, Map}) ->
    {ok, Id} = maps:find(id, Map),
    Id.

%% @spec matches( Account::account(), MP:: re:mp() ) -> bool()
%% @doc Check if any of the account fields matches the given regular expression
matches({account, Map}, MP) ->
    First = maps:iterator(Map),
    Loop = fun(F,I) ->
        case maps:next(I) of
            none -> false;
            {id, _, Next} ->
                F(F, Next);
            {_, V, Next}  ->
                case re:run(V, MP) of
                    {match, _} -> true;
                    _ -> F(F, Next)
                end
        end
           end,
    Loop(Loop,First).

%% ----------------------------------------------------------------------------
%% The internal functions
%% ----------------------------------------------------------------------------

decode(Port, Pwd) ->
    Port ! {self(), {command, [Pwd, "\n"] }},
    read(Port, []).

encode(Port, Pwd, Accounts) ->
    Port ! {self(), {command, [Pwd, "\n"] }},
    Port ! {self(), {command, [Pwd, "\n"] }},
    write(Port, lists:reverse(Accounts)).

read(Port, Accumulator) ->
    receive
        {Port , {data, {eol, Line}}} ->
            case iolist_to_binary(Line) of
                <<"---">> ->
                    read(Port, [{account, #{ id => length(Accumulator) + 1}}|Accumulator]); % new data structure
                <<"t: ", Value/binary>> ->
                    read(Port, add_field(Accumulator, title, Value));
                <<"url: ", Value/binary>> ->
                    read(Port, add_field(Accumulator, url, Value));
                % username
                <<"u: ", Value/binary>> ->
                    read(Port, add_field(Accumulator, username, Value));
                % password
                <<"p: ", Value/binary>> ->
                    read(Port, add_field(Accumulator, password, Value));
                % notes
                <<"n: ", Value/binary>> ->
                    read(Port, add_field(Accumulator, notes, Value));
                % other
                <<"o: ", Value/binary>> ->
                    read(Port, add_field(Accumulator, other, Value));
                % discard unmanaged lines
                _ ->
                    read(Port, Accumulator)
            end;
        {Port, {exit_status, 0}} ->
            lists:reverse(Accumulator);
        {'EXIT', Port, _Reason} ->
            exit(1)
    end.

write(Port,[]) -> port_close(Port), ok;
write(Port, Accounts) ->
    [H|Tail] = Accounts,
    ?LOG_DEBUG(#{ what => H, log => trace, level => debug}),
    {account, M} = H,
    Port ! { self(), {command, "---\n"}},
    Send = fun(Key, Field) ->
        Port ! { self(), {command, io_lib:format("~s: ~s~n", [ Field, maps:get(Key, M, "") ])}}
           end,
    Send(title, "t"),
    Send(url, "url"),
    Send(username, "u"),
    Send(password, "p"),
    Send(notes, "n"),
    Send(other, "o"),
    write(Port, Tail).

%% @doc Adds a field to the account on top of accounts collection
add_field(Accumulator, Field, Value) ->
    [{account, Map} | Tail] = Accumulator,
    [{account, maps:put(Field, Value, Map) } | Tail].

%% @spec (FileName :: chars(), DateTime :: datetime()) -> string()
%% @doc Provides a backup file name, given a filename
%% @todo: convert from FileName to FilePath
backup_name(FileName, {{Year,Month,Day},{Hours,Minutes, Seconds}}) ->
    lists:flatten(io_lib:format("~4..0B~2..0B~2..0B~2..0B~2..0B~2..0B_~s.bkp", [Year, Month, Day, Hours, Minutes, Seconds, FileName])).

%% ----------------------------------------------------------------------------
%% Entry point
%% ----------------------------------------------------------------------------
main(Args) ->
    [FileName|Password] = Args,
    Accounts = load(FileName, Password),
    io:format("Accounts:~n~p~n~n", [Accounts]).

%% ----------------------------------------------------------------------------
%% Test cases
%% ----------------------------------------------------------------------------
matches_test_() ->
    Account =
        {account,
            #{ title => "Google",
                url => "http://www.google.com",
                username => "c.tomasin@gmail,com",
                password => "uno due tre"}
        },
    [
        ?_assert(begin
                     {ok, MP} = re:compile("calippo"),
                     matches(Account, MP) =:= false end
        ),
        ?_assert(begin
                     {ok, MP} = re:compile("google"),
                     matches(Account, MP) =:= true end
        ),
        ?_assert(begin
                     {ok, MP} = re:compile("DUE",[caseless]),
                     matches(Account, MP) =:= true end
        )

    ].

matches_list_accounts_comprehension_test_() ->
    Accounts = [

        {account,
            #{title => "Amazon",
                url => "http://www.amazon.com",
                username => "carlo.romasin",
                password => "one two three"}},
        {account,
            #{ title => "Google",
                url => "http://www.google.com",
                username => "c.tomasin@gmail,com",
                password => "uno due tre"}
        }
    ],
    [
        ?_assert(begin
                     {ok, MP} = re:compile("google", [caseless]),
                     FA = [ X || X <- Accounts, ?MODULE:matches(X, MP) ],
                     length(FA) =:= 1
                 end),
        ?_assert(begin
                     {ok, MP} = re:compile("nomatch", [caseless]),
                     FA = [ X || X <- Accounts, ?MODULE:matches(X, MP) ],
                     length(FA) =:= 0
                 end)
    ].

backup_name_test_() ->
    FileName = "file.of.accounts",
    BackupName = backup_name(FileName, {{2020,7, 9}, {9,8,7}}),
    [
        ?_assertEqual("20200709090807_file.of.accounts.bkp", BackupName)
    ].
