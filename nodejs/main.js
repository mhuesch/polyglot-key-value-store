var fs = require('fs')
var BSON = require('bson')
var bson = new BSON();

class KVStore {

  constructor(filepath = null) {
    if (filepath === null) {
      // random extension
      // low quality entropy, but we don't care yet
      var randomExt = Math.random().toString(36).substr(2)
      filepath = './db-' + randomExt
      console.log('no filepath given, using `' + filepath + '`')
      this.fd = fs.openSync(filepath, 'wx+')
      this.dict = {}
      this.size = fs.statSync(filepath).size

    } else {
      // existing dictionary file
      this.fd = fs.openSync(filepath, 'r+')
      this.size = fs.statSync(filepath).size

      // fill in dictionary
      this.dict = extractDict(this.fd, this.size)
    }
  }

  get(key) {
    if (key in this.dict) {

      // read from offset
      var valOffset = this.dict[key].valOffset
      var valLen = this.dict[key].valLen
      var valBuf = new Buffer(valLen)
      var bytesRead = fs.readSync(this.fd, valBuf, 0, valLen, valOffset)
      if (bytesRead != valLen) {
        console.error("get: bad read at offset " + valOffset + ".")
        return -1
      }
      return bson.deserialize(valBuf).val

    } else {

      console.error("key `" + key + "` not in dictionary")

    }
  }

  set(key,val) {

    var headerLen = 8
    var headerBuf = new Buffer(headerLen)
    //
    var keyBuf = bson.serialize({key: key})
    var keyLen = keyBuf.byteLength
    var valBuf = bson.serialize({val: val})
    var valLen = valBuf.byteLength
    //
    headerBuf.writeInt32BE(keyLen,0)
    headerBuf.writeInt32BE(valLen,4)

    var bytesWritten = fs.writeSync(this.fd, headerBuf, 0, headerLen, this.size)
    if (bytesWritten != headerLen) {
      console.error("set: bad write at offset " + this.size + ".")
      process.exit(1)
    }
    //
    bytesWritten = fs.writeSync(this.fd, keyBuf, 0, keyLen, this.size + headerLen)
    if (bytesWritten != keyLen) {
      console.error("set: bad write at offset " + (this.size + headerLen) + ".")
      process.exit(1)
    }
    //
    bytesWritten = fs.writeSync(this.fd, valBuf, 0, valLen, this.size + headerLen + keyLen)
    if (bytesWritten != valLen) {
      console.error("set: bad write at offset " + (this.size + headerLen + keyLen) + ".")
      process.exit(1)
    }

    // TODO: rollback partial writes on failure?

    this.dict[key] = { valOffset: this.size + headerLen + keyLen
                     , valLen: valLen }
    this.size = this.size + headerLen + keyLen + valLen

    return true

  }

  del(key) {
    console.error("delete is unimplemented")
  }

}

function extractDict(fd, size) {
  var offset = 0
  var dict = {}
  var headerLen = 8
  var headerBuf = new Buffer(headerLen)
  while (offset < size) {

    var bytesRead = fs.readSync(fd, headerBuf, 0, headerLen, offset)
    if (bytesRead != headerLen) {
      console.error("extractDict: headerBuf: bad read at offset " + offset + ".")
      process.exit(1)
    }

    var keyLen = headerBuf.readInt32BE(0)
    var valLen = headerBuf.readInt32BE(4)

    var keyBuf = new Buffer(keyLen)
    bytesRead = fs.readSync(fd, keyBuf, 0, keyLen, offset+headerLen)
    if (bytesRead != keyLen) {
      console.error("extractDict: keyBuf: bad read at offset " + (offset+headerLen) + ".")
      process.exit(1)
    }
    var key = bson.deserialize(keyBuf).key
    var valStruct = { valOffset: offset + headerLen + keyLen
                    , valLen: valLen }
    dict[key] = valStruct

    offset = offset + headerLen + keyLen + valLen
  }

  return dict
}

exports.KVStore = KVStore
