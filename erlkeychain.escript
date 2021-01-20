#!/usr/bin/env escript
%%! -noinput -pa _build/default/lib/erlkeychain/ebin -pa _build/default/lib/cecho/ebin +A 50

%% Read the LICENSE file
main([]) ->
    erlkeychain:main([]).
