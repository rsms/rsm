#!/usr/bin/env node
import { readFile } from "node:fs/promises"
import { resolve } from "node:path"

const root = new URL(".", import.meta.url)
const wasmPath = resolve(process.argv[2] || "out/safe/rsm.wasm")

function assert(cond, msg) {
    if (!cond)
        throw new Error(msg)
}

async function importRSMJS() {
    const src = await readFile(new URL("etc/rsm.js", root), "utf8")
    const url = `data:text/javascript;charset=utf-8,${encodeURIComponent(src)}`
    return import(url)
}

function arrayBufferFromBuffer(buf) {
    return buf.buffer.slice(buf.byteOffset, buf.byteOffset + buf.byteLength)
}

const { createRSMInstance } = await importRSMJS()
const wasmData = arrayBufferFromBuffer(await readFile(wasmPath))
const wasmResponse = new Response(wasmData, {
    headers: { "Content-Type": "application/wasm" },
})
const rsm = await createRSMInstance(Promise.resolve(wasmResponse), 64)

const rom = rsm.assemble("fun main(i32) i32 { R0 = R0 * 2; ret; }", "double.rsm")
assert(rom.imgptr !== 0, "assemble returned null ROM pointer")
assert(rom.imgsize > 0, "assemble returned empty ROM")
assert(rom.imgmemsize >= rom.imgsize, "ROM allocation is smaller than image")

const formatted = rsm.fmtprog(rom)
assert(formatted.includes("R0"), "formatted program does not mention R0")

const result = rsm.exec(rom, 6)
assert(Number(result[0]) === 12, `double(6) got ${result[0]}, want 12`)
rom.dispose()
assert(rom.imgptr === 0, "dispose did not clear ROM pointer")

for (let i = 0; i < 8; i++) {
    const r = rsm.assemble(`fun main() i32 { R0 = ${i}; ret; }`, `repeat-${i}.rsm`)
    const out = rsm.exec(r)
    assert(Number(out[0]) === i, `repeat ${i} got ${out[0]}, want ${i}`)
    r.dispose()
}

let failed = false
try {
    rsm.assemble("fun broken( {", "broken.rsm")
} catch (_) {
    failed = true
}
assert(failed, "invalid source did not report an error")

console.log(`ok ${wasmPath}`)
