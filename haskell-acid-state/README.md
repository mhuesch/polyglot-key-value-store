# haskell

- install [stack](https://docs.haskellstack.org/en/stable/README/)
- `cd haskell`
- to build: `stack build`
- to execute: `stack exec kvstore`

example transcript:
```
$ stack build && stack exec kvstore
----------------------------------------
welcome to mhueschen's key-value store
available commands:
set <key> value>
get <key>
delete <key>
all-keys
help
quit
----------------------------------------
> all-keys
"a"
"b"
"xxxxxxx"
> delete xxxxxxx
> all-keys
"a"
"b"
> set c 9
> get c
Just "9"
> get a
Just "a"
> set a fishes
> get a
Just "fishes"
> all-keys
"a"
"b"
"c"
> delete a
> all-keys
"b"
"c"
> quit
```
