const ISIZE = 4 // sizeof(isize) (wasm32=4, wasm64=8)
const PTR = Symbol("ptr")

// UTF8 encoder & decoder
// note: nodejs has require("util").TextEncoder & .TextDecoder
const txt_enc = new TextEncoder("utf-8")
const txt_dec = new TextDecoder("utf-8")

class RSMInstance {
  constructor(memory) {
    this.memory = memory
    this.mem_u8 = new Uint8Array(memory.buffer)
    this.mem_i32 = new Int32Array(memory.buffer)
    this.mem_u32 = new Uint32Array(memory.buffer)
    if (typeof BigUint64Array != "undefined") {
      this.mem_u64 = new BigUint64Array(memory.buffer)
      this.u64 = (ptr) => this.mem_u64[ptr >>> 3]
      this.R = (iregno) => this.u64(this.iregvp + (iregno * 8))
      this.setR = (iregno, v) => {
        this.mem_u64[(this.iregvp + (iregno * 8)) >>> 3] = BigInt(v)
      }
    } else {
      this.R = (iregno) => this.u32(this.iregvp + (iregno * 8))
      this.setR = (iregno, v) => {
        this.setU32(this.iregvp + (iregno * 8), v)
      }
    }
    this.instance = null // set by wasm_load
    this.tmpbufcap = 0
    this.tmpbufp = 0
  }

  init(instance) {
    this.instance = instance
    instance.exports.winit(this.memory.buffer.byteLength)
    this.iregvp = this.instance.exports.wvmiregs()
  }

  log(message) { console.log(message) }

  memalloc(size) {
    const p = this.instance.exports.wmalloc(size)
    if (p == 0) throw new Error("out of memory")
    return p
  }
  memresize(p, currsize, newsize) {
    p = this.instance.exports.wmresize(p, currsize, newsize)
    if (p == 0) throw new Error("out of memory")
    return p
  }
  memfree(p, size) { this.instance.exports.wmfree(p, size) }

  u8(ptr)  { return this.mem_u8[ptr] }

  i32(ptr) { return this.mem_i32[ptr >>> 2] }
  u32(ptr) { return this.mem_u32[ptr >>> 2] }

  setI32(ptr, v) { this.mem_u32[ptr >>> 2] = v }
  setU32(ptr, v) { this.mem_u32[ptr >>> 2] = (v >>> 0) }

  isize(ptr) { return this.mem_i32[ptr >>> 2] }
  usize(ptr) { return this.mem_u32[ptr >>> 2] >>> 0 }

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

  compile(source, filename) {
    let srcbuf = txt_enc.encode(String(source)) // Uint8Array

    if (!this.srcnamecap) {
      this.srcnamecap = 128
      this.srcnamep = this.memalloc(this.srcnamecap)
    }
    this.setCStr(this.srcnamep, this.srcnamecap, filename || "input")

    let [tmpbufp, tmpbufcap] = this.tmpbuf(srcbuf.length)
    let tmpbuflen = this.copyToWasm(tmpbufp, tmpbufcap, srcbuf)

    // rptr : struct compile_result {
    //   usize   c;
    //   rinstr* v;
    //   const char* nullable errmsg;
    // }
    const rptr = this.instance.exports.wcompile(this.srcnamep, tmpbufp, tmpbuflen)
    const ilen = this.u32(rptr)
    const iptr = this.u32(rptr + ISIZE)
    const errmsgp = this.u32(rptr + ISIZE*2)
    if (errmsgp)
      throw new Error(this.cstr(errmsgp))

    return { filename, length: ilen, [PTR]: iptr }
  }

  vmfmt(vmcode) {
    // usize wfmtprog(char* buf, usize bufcap, rinstr* nullable inv, u32 inc)
    let [tmpbufp, tmpbufcap] = this.tmpbuf(vmcode.length * 128)
    let len = this.instance.exports.wfmtprog(tmpbufp, tmpbufcap, vmcode[PTR], vmcode.length)
    return this.str(tmpbufp, len)
  }

  vmexec(vmcode, ...args) { // => [R0 ... R7]
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
    this.instance.exports.wvmexec(this.iregvp, vmcode[PTR], vmcode.length)
    return this.Rarray(8)
  }

  vmfree(vmcode) {
    if (vmcode[PTR] == 0)
      return
    this.memfree(vmcode[PTR], vmcode.length * 4)
    vmcode.length = 0
    vmcode[PTR] = 0
  }
}

let wasm_mod = null

function createNthInstance(instance, import_obj, nmempages) {
  return WebAssembly.instantiate(wasm_mod, import_obj).then(winstance => {
    instance.init(winstance)
    return instance
  })
}

// createRSMInstance creates a new RSM WASM module instance and initializes it.
// nmempages is optional and specifies memory pages to allocate for the wasm instance.
// One page is 65536 B (64 ikB).
export async function createRSMInstance(urlorpromise, nmempages) {
  const memory = new WebAssembly.Memory({ initial: nmempages||64 }) // 4MB by default
  const instance = new RSMInstance(memory)
  const import_obj = {
    env: {
      memory,
      log1: (strp, len) => { instance.log(instance.str(strp, len)) },
    },
  }
  if (wasm_mod)
    return createNthInstance(instance, import_obj, nmempages)
  const fetchp = urlorpromise instanceof Promise ? urlorpromise : fetch(urlorpromise)
  const istream = WebAssembly.instantiateStreaming
  return (
    istream ? istream(fetchp, import_obj) :
    fetchp.then(r => r.arrayBuffer()).then(buf => WebAssembly.instantiate(buf, import_obj))
  ).then(r => {
    wasm_mod = r.module
    instance.init(r.instance)
    return instance
  })
}
