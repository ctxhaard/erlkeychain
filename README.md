# erlkeychain

See [pykeychain project README.m](https://github.com/ctxhaard/erlkeychain) to more details of archive file format and encryption.

## To build: 
```sh
$ rebar3 compile
```

## To run:
```sh
$ ./erlkeychain.escript
```

## To-do

- rewrite with gen_server;
- allow passing FilePath as script argument;
- modify backup file to manage file path instead of file name;
