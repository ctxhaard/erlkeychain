-module(erlkc_account).
-author('c.tomasin@gmail.com').

-export([load/2, main/1]).

load(_FilePath, Pwd) ->
    process_flag(trap_exit, true),
    % openssl enc -d -aes-256-cbc -md sha256 -in <file>
    % passo la password nello standard input di openssl, in modo
    % che la password non sia visibile nella lista dei processi
    Port = open_port(
        {spawn, <<"openssl enc -d -aes-256-cbc -md sha256 -in archive.protected">>},
        [ use_stdio, exit_status, {line, 255} ]
    ),
    decode(Port, Pwd).

decode(Port, Pwd) ->
    Port ! {self(), {command, Pwd ++ "\n" }},
    loop(Port, []).

loop(Port, Accumulator) ->
    receive
        {Port , {data, {eol, Line}}} ->
            % TODO: parse the line
            case Line of
                "---" ->  
                    loop(Port, [{account, []}|Accumulator]); % new data structure
                % title
                [$t|[$:|[$\s|Value]]] ->
                    loop(Port, add_field(Accumulator, title, Value));
                % url
                [$u|[$r|[$l|[$:|[$\s|Value]]]]] ->
                    loop(Port, add_field(Accumulator, url, Value));
                % username
                [$u|[$:|[$\s|Value]]] ->
                    loop(Port, add_field(Accumulator, username, Value));
                % password
                [$p|[$:|[$\s|Value]]] ->
                    loop(Port, add_field(Accumulator, password, Value));
                % notes
                [$n|[$:|[$\s|Value]]] ->
                    loop(Port, add_field(Accumulator, notes, Value));
                % other
                [$o|[$:|[$\s|Value]]] ->
                    loop(Port, add_field(Accumulator, other, Value))
            end;
        {Port, {exit_status, 0}} ->
            %io:format("exiting~n~n"),
            Accumulator;
        {'EXIT', Port, _Reason} ->
            exit(1)
    end.

    % [ 
    %     {account, [
    %         {title, "This is the title"}, 
    %         {url, "http://blah.org"}, 
    %         {username, "mario.rossi"}, 
    %         {password, "1234567"}, 
    %         {note, "Test account"}, 
    %         {others, "Dunno what this field means"}]},

    %     {account, [
    %         {title, "Yet another title"}, 
    %         {url, "http://youp.org"}, 
    %         {username, "andrea.bianchi"}, 
    %         {password, "abcdef"}, 
    %         {note, "Test account #2"}, 
    %         {others, "Dunno what this field means #2"}]} 

    % ].

add_field(Accumulator, Field, Value) ->
    [{account, List} | Tail] = Accumulator,
    [{account, [{Field, Value} | List] } | Tail].

main(Args) ->
    [FileName|Password] = Args,
    load(FileName, Password).