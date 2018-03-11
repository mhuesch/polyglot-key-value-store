# shell commands

note: this likely only works on mac os, as sed's inplace (`-i`) syntax differs across platforms.

usage
```
$ source db.sh
$ db_set a 1
$ db_get a
# => 1
$ db_set a 2
$ db_get a
# => 2
$ db_del a
$ db_get a
# =>
#    ^ nothing
```
