var fs = require('fs')

class KVStore {

  constructor(filepath = null) {
    if (filepath === null) {
      // random extension
      // low quality entropy, but we don't care yet
      var randomExt = Math.random().toString(36).substr(2)
      filepath = './db-' + randomExt
      console.log('no filepath given, using `' + filepath + '`')
      this.fd = fs.openSync(filepath, 'wx+')
    } else{
      this.fd = fs.openSync(filepath, 'r+')
    }
    this.dict = {}
  }

  get(key) {
    return this.dict[key]
  }

  set(key,value) {
    var buf = key.toString() + '|' + value.toString() + '\n'
    console.log(buf)
    fs.writeSync(this.fd, buf)
    this.dict[key] = value
  }

  del(key) {
    if (!(delete this.dict[key])) {
      console.error('del failed for key `' + key.toString() + '`')
    }
  }

}

exports.KVStore = KVStore
