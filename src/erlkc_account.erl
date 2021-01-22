-module(erlkc_account).
-author('c.tomasin@gmail.com').

-include_lib("eunit/include/eunit.hrl").

-export([load/2, matches/2, main/1]).


load(_FilePath, Pwd) ->
    process_flag(trap_exit, true),
    % openssl enc -d -aes-256-cbc -md sha256 -in <file>
    % passo la password nello standard input di openssl, in modo
    % che la password non sia visibile nella lista dei processi
    Port = open_port(
        {spawn, <<"openssl enc -d -aes-256-cbc -md sha256 -in archive.protected">>},
        [ use_stdio, stderr_to_stdout, exit_status, {line, 255} ]
    ),
    decode(Port, Pwd).

decode(Port, Pwd) ->
    Port ! {self(), {command, [Pwd, "\n"] }},
    receive_loop(Port, []).

receive_loop(Port, Accumulator) ->
    receive
        {Port , {data, {eol, Line}}} ->
            case iolist_to_binary(Line) of
                <<"---">> ->
                    receive_loop(Port, [{account, #{}}|Accumulator]); % new data structure
                <<"t: ", Value/binary>> ->
                    receive_loop(Port, add_field(Accumulator, title, Value));
                <<"url: ", Value/binary>> ->
                    receive_loop(Port, add_field(Accumulator, url, Value));
                % username
                <<"u: ", Value/binary>> ->
                    receive_loop(Port, add_field(Accumulator, username, Value));
                % password
                <<"p: ", Value/binary>> ->
                    receive_loop(Port, add_field(Accumulator, password, Value));
                % notes
                <<"n: ", Value/binary>> ->
                    receive_loop(Port, add_field(Accumulator, notes, Value));
                % other
                <<"o: ", Value/binary>> ->
                    receive_loop(Port, add_field(Accumulator, other, Value));
                % discard unmanaged lines
                _ ->
                    receive_loop(Port, Accumulator)
            end;
        {Port, {exit_status, 0}} ->
            lists:reverse(Accumulator);
        {'EXIT', Port, _Reason} ->
            exit(1)
    end.

add_field(Accumulator, Field, Value) ->
    [{account, Map} | Tail] = Accumulator,
    [{account, maps:put(Field, Value, Map) } | Tail].

matches({account, Map}, MP) ->
    First = maps:iterator(Map),
    Loop = fun(F,I) ->
        case maps:next(I) of
            none -> false;
            {_K, V, Next} -> 
                case re:run(V, MP) of
                    {match, _} -> true;
                    _ -> F(F, Next)
                end
        end
    end,
    Loop(Loop,First).

% called by ERTS when run as a script
main(Args) ->
    [FileName|Password] = Args,
    Accounts = load(FileName, Password),
    io:format("Accounts:~n~p~n~n", [Accounts]).

matches_do_not_match_test_() ->
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
