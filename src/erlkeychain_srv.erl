-module(erlkeychain_server).
-export([start/0]).

start() ->
    spawn_link(fun() ->
            io:format("At least server was started!~n")
         end).