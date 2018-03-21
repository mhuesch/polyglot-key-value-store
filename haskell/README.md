# haskell

- install [stack](https://docs.haskellstack.org/en/stable/README/)
- to build: `stack build`

## example run

see `test_run.txt`. kvstore can handle stdin input, like so: `cat test_run.txt | stack exec -- kvstore`

once a database has been created, it can be reused like so:  `cat test_run.txt | stack exec -- kvstore -d $DB_PATH`

## design

based on [Basho's BitCask](http://basho.com/wp-content/uploads/2015/05/bitcask-intro.pdf). the file contains a sequence of records of the form

```
+--------+--------+-----------------+---------------------+
| keyLen | valLen | key             | val                 |
+--------+--------+-----------------+---------------------+
  8        8        keyLen            valLen
  ^ bytes
```

a mapping is maintained in-memory (using a `Data.Map.Map`) from Keys to ValStructs (which contains the offset in the file at which the value starts, and the length of the value in bytes). this setup allows us to "get" a value with only one disk seek and a read of a known number of bytes. "set"-ing a value requires us to serialize the keys & values, count their lengths, append them onto the end of the file, and update the in-memory dictionary. thus it requires only one disk seek, to the end of the file, and a write.

currently we expose a library. it would be simple to create a command line or HTTP interface to the store.

other TODOs:
- support datatypes other than strings
- support deletion of keys (using tombstones)
- support compaction of the db file
- support multiple files
