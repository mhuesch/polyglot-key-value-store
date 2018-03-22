# nodejs

- `npm install`

## example usage

```
$ node
> var kvstore = require('./main.js')
> var kvs = new kvstore.KVStore()
> kvs.get(2)
key `2` not in dictionary
undefined
> kvs.set(2,9)
true
> kvs.get(2)
9
```
