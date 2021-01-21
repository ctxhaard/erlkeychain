erlkeychain
=====

An escript

Build
-----

    $ rebar3 compile

Run
---

    $ ./erlkeychain.escript


## TODO

- debugger;
- unittests;
- test binary pattern matching instead of list pattern matching when parsing single lines;
- list pattern matching is so horrible when used to match a string prefix longer than 1 char!
- how to avoid ```openssl``` std_err output to be seen?
- is there a better option than ```Pwd ++ "\n"```?
- strings formatting and binary strings concatenation need some insight;
