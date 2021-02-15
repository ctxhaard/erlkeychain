# Erlang Keychain manager based on ncurses

## Notes

This application is written with MVC pattern in mind.
In order de-couple the model from the View and The Controller
a ```gen_event``` process is used.

## Build
```shell
$ rebar3 compile
```

## Launch
```shell
$ rebar3 shell
```

## Release
```shell
$ rebar3 release
# to test the release
_build/default/rel/keychain/bin/keychain console
```