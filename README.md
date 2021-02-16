# Erlang Keychain manager based on ncurses

## Notes

This application is written with MVC pattern in mind.
In order de-couple the model from the View and The Controller
a ```gen_event``` process is used.

## Build
```shell
$ rebar3 compile
```

## Standalone Launch
```shell
$ rebar3 shell
```

## Distributed Launch

### Server Example
```shell
erl -sname uno -set-cookie mycookie -pa  _build/default/lib/*/ebin -config config/sys.config
```
```erlang
(uno@CV-LAP-CT)1> kc_app:start(null, [server]).
{ok,<0.102.0>}
```
### Client Example
```shell
erl -sname due -set-cookie mycookie -pa  _build/default/lib/*/ebin -config config/sys.config
```
```erlang
net_adm:ping('uno@CV-LAP-CT')
kc_app:start(null, [client]).
```
## Release
```shell
$ rebar3 release
# to test the release
_build/default/rel/keychain/bin/keychain console
```