const ISIZE = 4 // sizeof(isize) (wasm32=4, wasm64=8)
const PTRSIZE = 4 // sizeof(void*) (wasm32=4, wasm64=8)
const PTR = Symbol("ptr")

// UTF8 encoder & decoder
// note: nodejs has require("util").TextEncoder & .TextDecoder
const txt_enc = new TextEncoder("utf-8")
const txt_dec = new TextDecoder("utf-8")

class ROM {
  constructor(rsm, filename, imgptr, imgsize) {
    this.rsm = rsm // :RSMInstance
    this.imgptr = imgptr
    this.imgsize = imgsize
    this.filename = filename
  }

  dispose() {
    if (this.imgptr == 0)
      return
    //console.log(`ROM@${this.imgptr.toString(16)}.dispose`)
    this.rsm.memfree(this.imgptr, this.imgsize)
    this.imgptr = 0
    this.imgsize = 0
    this.rsm = null
  }

  data() { // :Uint8Array
    return this.rsm.mem_u8.subarray(this.imgptr, this.imgptr + this.imgsize)
  }
}

class RSMInstance {
  constructor(memory) {
    this.memory = memory
    this.mem_u8 = new Uint8Array(memory.buffer)
    this.mem_i32 = new Int32Array(memory.buffer)
    this.mem_u32 = new Uint32Array(memory.buffer)
    if (typeof BigUint64Array != "undefined") {
      this.mem_u64 = new BigUint64Array(memory.buffer)
      this.mem_i64 = new BigInt64Array(memory.buffer)
      this.u64 = (ptr) => this.mem_u64[ptr >>> 3]
      this.i64 = (ptr) => this.mem_i64[ptr >>> 3]
      this.setU64 = (ptr, v) => { this.mem_u64[ptr >>> 3] = BigInt(v >>> 0) }
      this.setI64 = (ptr, v) => { this.mem_i64[ptr >>> 3] = BigInt(v | 0) }
      this.R = (iregno) => this.u64(this.iregvp + (iregno * 8))
      this.setR = (iregno, v) => {
        this.mem_u64[(this.iregvp + (iregno * 8)) >>> 3] = BigInt(v)
      }
    } else {
      this.u64 = this.u32
      this.i64 = this.i32
      this.setU64 = this.setU32
      this.setI64 = this.setI32
      this.R = (iregno) => this.u32(this.iregvp + (iregno * 8))
      this.setR = (iregno, v) => {
        this.setU32(this.iregvp + (iregno * 8), v)
      }
    }
    this.instance = null // wasm instance, set by init()
    this.api = null // ==this.instance.exports, set by init()
    this.tmpbufcap = 0
    this.tmpbufp = 0
  }

  init(instance) {
    this.instance = instance
    this.api = instance.exports
    if (!this.api.winit(this.memory.buffer.byteLength))
      throw new Error("rsm wasm module failed to initialize")
    this.iregvp = this.api.wvmiregs()
  }

  log(message) { console.log(message) }

  memalloc(size) {
    const p = this.api.wmalloc(size)
    if (p == 0) throw new Error("out of memory")
    return p
  }
  memresize(p, currsize, newsize) {
    p = this.api.wmresize(p, currsize, newsize)
    if (p == 0) throw new Error("out of memory")
    return p
  }
  memfree(p, size) { this.api.wmfree(p, size) }

  u8(ptr)  { return this.mem_u8[ptr] }

  i32(ptr) { return this.mem_i32[ptr >>> 2] }
  u32(ptr) { return this.mem_u32[ptr >>> 2] }

  setI32(ptr, v) { this.mem_u32[ptr >>> 2] = (v | 0) }
  setU32(ptr, v) { this.mem_u32[ptr >>> 2] = (v >>> 0) }

  isize(ptr) { return this.mem_i32[ptr >>> 2] }
  usize(ptr) { return this.mem_u32[ptr >>> 2] }

  uintptr(ptr) { return this.mem_u32[ptr >>> 2] }
  setUintptr(ptr) { this.mem_u32[ptr >>> 2] = (v >>> 0) }

  Rarray(count) {
    let a = []
    for (let i = 0; i < count; i++)
      a[i] = this.R(i)
    return a
  }

  str(ptr, len) {
    return txt_dec.decode(new DataView(this.memory.buffer, ptr, len))
  }
  cstr(ptr) {
    const len = this.mem_u8.indexOf(0, ptr) - ptr
    return txt_dec.decode(new DataView(this.memory.buffer, ptr, len))
  }
  setStr(dstptr, cap, jsstr) {
    let buf = txt_enc.encode(String(jsstr))
    return this.copyToWasm(dstptr, cap, buf)
  }
  setCStr(dstptr, cap, jsstr) {
    let buf = txt_enc.encode(String(jsstr)+"\0")
    return this.copyToWasm(dstptr, cap, buf)
  }

  copyToWasm(dstptr, cap, buf) {
    if (buf.length > cap)
      buf = buf.subarray(0, cap)
    this.mem_u8.set(buf, dstptr)
    return buf.length
  }

  tmpbuf(minsize) {
    if (this.tmpbufcap < minsize) {
      const cap = Math.max(4096, minsize)
      this.tmpbufp = (
        this.tmpbufcap == 0 ? this.memalloc(cap) :
        this.memresize(this.tmpbufp, this.tmpbufcap, cap) )
      this.tmpbufcap = cap
    }
    return [this.tmpbufp, this.tmpbufcap]
  }

  // createROM(imgbuf :ArrayBuffer|Uint8Array, filename? :string) :ROM
  createROM(imgbuf, filename) {
    let buf = (imgbuf instanceof Uint8Array) ? imgbuf : new Uint8Array(imgbuf)
    let imgsize = buf.length
    let imgptr = this.memalloc(imgsize)
    this.mem_u8.set(buf, imgptr)
    return new ROM(this, filename, imgptr, imgsize)
  }

  // assemble(source :string|Uint8Array, filename? :string) :ROM
  assemble(source, filename) {
    let srcbuf = (source instanceof Uint8Array) ? source : txt_enc.encode(String(source))

    if (!this.srcnamecap) {
      this.srcnamecap = 128
      this.srcnamep = this.memalloc(this.srcnamecap)
    }
    this.setCStr(this.srcnamep, this.srcnamecap, filename || "input")

    let [tmpbufp, tmpbufcap] = this.tmpbuf(srcbuf.length)
    let tmpbuflen = this.copyToWasm(tmpbufp, tmpbufcap, srcbuf)

    // struct cresult { rromimg* imgptr; usize imgsize; const char* errmsg; }
    let p = this.api.wcompile(this.srcnamep, tmpbufp, tmpbuflen)
    const imgptr = this.uintptr(p) ; p += PTRSIZE
    const imgsize = this.usize(p) ; p += ISIZE
    const errmsgptr = this.uintptr(p)
    if (errmsgptr)
      throw new Error(this.cstr(errmsgptr))

    return new ROM(this, filename, imgptr, imgsize)
  }

  // fmtprog(rom :ROM) :string
  fmtprog(rom) {
    let [tmpbufp, tmpbufcap] = this.tmpbuf(rom.imgsize * 8)
    let canResize = true
    while (canResize) {
      let len = this.api.wfmtprog(tmpbufp, tmpbufcap, rom.imgptr, rom.imgsize)
      if (len < 0)
        throw new Error(`invalid ROM data`)
      if (len > tmpbufcap) {
        [tmpbufp, tmpbufcap] = this.tmpbuf(len)
        canResize = false // avoid infinite loop in case of bug
        continue
      }
      return this.str(tmpbufp, len)
    }
  }

  // exec(rom :ROM, ...args :int[]) :int[8]
  exec(rom, ...args) { // => [R0 ... R7]
    // TODO: refactor: create an execution instance
    // Move "this.iregvp = this.api.wvmiregs()" to it, too, along with R functions.
    //
    if (args.length > 8)
      throw new Error(`too many arguments`)
    for (let i = 0; i < args.length; i++) {
      let arg = args[i]
      if (typeof arg != "bigint") {
        if (typeof arg != "number")
          throw new Error(`argument ${i+1} (${typeof arg}) is not a number`)
        if (arg > 0xffffffff)
          throw new Error(`argument ${i+1} is too large`)
      }
      this.setR(i, arg)
    }
    let err = this.api.wvmexec(this.iregvp, rom.imgptr, rom.imgsize)
    if (err)
      throw new Error(`wvmexec rerror#${err}`)
    return this.Rarray(8)
  }
}

let wasm_mod = null

function createNthInstance(rsm, import_obj, nmempages) {
  return WebAssembly.instantiate(wasm_mod, import_obj).then(wasm_instance => {
    rsm.init(wasm_instance)
    return rsm
  })
}

function unixtime(rsm, secp, nsecp) { // time.c
  const t = Date.now()
  rsm.setI64(secp, t / 1000)
  rsm.setU64(secp, (t % 1000) * 1000000)
  return 0;
}

function wasm_nanotime() { // time.c
  return performance.now() * 1000000
}

// createRSMInstance creates a new RSM WASM module instance and initializes it.
// nmempages is optional and specifies memory pages to allocate for the wasm instance.
// One page is 65536 B (64 ikB).
export async function createRSMInstance(urlorpromise, nmempages) {
  if (!nmempages) nmempages = 1024 // 65MB by default
  const memory = new WebAssembly.Memory({ initial: nmempages })
  const rsm = new RSMInstance(memory)
  const import_obj = {
    env: {
      memory,
      wasm_log(strp, len) { rsm.log(rsm.str(strp, len)) },
      unixtime: unixtime.bind(null, rsm),
      wasm_nanotime,
    },
  }
  if (wasm_mod)
    return createNthInstance(rsm, import_obj, nmempages)
  const fetchp = urlorpromise instanceof Promise ? urlorpromise : fetch(urlorpromise)
  const istream = WebAssembly.instantiateStreaming
  return (
    istream ? istream(fetchp, import_obj) :
    fetchp.then(r => r.arrayBuffer()).then(buf => WebAssembly.instantiate(buf, import_obj))
  ).then(r => {
    wasm_mod = r.module
    rsm.init(r.instance)
    return rsm
  })
}
