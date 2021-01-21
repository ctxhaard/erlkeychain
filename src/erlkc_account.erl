-module(erlkc_account).

-export([load/2, main/1]).

-define(OPENSSL, <<"openssl">>).

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
    _Accounts = loop(Port, []),
    io:format("Accounts: ~p~n", [_Accounts]).

loop(Port, Accumulator) ->
    receive
        % {Port, {data, "enter aes-256-cbc decryption password:"}} ->
        %     io:format("Password prompt received!~n"),
        %     loop(Port, Pwd, Accumulator);
        % Blob -> io:format("Blob: ~p~n", [Blob]);
        {Port , {data, {eol, Line}}} ->
            % TODO: parse the line
            case Line of
                "---" ->  
                    loop(Port, [{account, []}|Accumulator]); % new data structure
                [$u|[$:|[$\s|Value]]] ->
                    [{account, List}|Tail] = Accumulator,
                    loop(Port, [{account, List ++ {username, Value}}|Tail]);
                [$t|[$:|[$\s|Value]]] ->
                    [{account, List}|Tail] = Accumulator,
                    loop(Port, [{account, List ++ {title, Value}}|Tail]);
                [$u|[$r|[$l|[$:|[$\s|Value]]]]] ->
                    [{account, List}|Tail] = Accumulator,
                    loop(Port, [{account, List ++ {url, Value}}|Tail]);
                [$p|[$:|[$\s|Value]]] ->
                    [{account, List}|Tail] = Accumulator,
                    loop(Port, [{account, List ++ {password, Value}}|Tail]);
                [$n|[$:|[$\s|Value]]] ->
                    [{account, List}|Tail] = Accumulator,
                    loop(Port, [{account, List ++ {notes, Value}}|Tail]);
                [$o|[$:|[$\s|Value]]] ->
                    [{account, List}|Tail] = Accumulator,
                    loop(Port, [{account, List ++ {other, Value}}|Tail])


            end;
        {Port, {exit_status, 0}} ->
            io:format("exiting~n~n"),
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

    main(Args) ->
        [FileName|Password] = Args,
        load(FileName, Password).