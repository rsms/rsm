// Implements ROM functions rsm_loadrom and rom_build
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#include "asm.h"

typedef u8 rrom_skind; // section kind
enum rrom_skind {
  RSM_ROM_DATA = 0x03,
  RSM_ROM_CODE = 0x04,

  rrom_skind_MIN = RSM_ROM_DATA,
  rrom_skind_MAX = RSM_ROM_CODE,
} RSM_END_ENUM(rrom_skind)

#define CODE_ALIGNMENT sizeof(rin_t) // alignment of CODE section body

// --------------------------------------------------------------------------------------
// ROM loader

#define LPARAMS  rrom_t* rom, const u8* p, const u8* end, u64 size
#define LARGS    rom, p, end, size
#define POFFS    ( (usize)((const void*)p - (const void*)rom->img) )
#define perr(fmt, args...) ({ \
  log("error while loading rom, at offset %zu: " fmt, POFFS, ##args); \
  rerr_invalid; })

static rerr_t leb_u64_read(u64* resultp, u32 nbit, const u8** input, const u8* inputend) {
  rerr_t err = rerr_invalid;
  u64 v = 0;
  u32 shift = 0;
  const u8* p = *input;
  while (p != inputend) {
    u64 b = *p++;
    v |= ((b & 0x7f) << shift);
    shift += 7;
    if ((b & 0x80) == 0) {
      err = 0;
      break;
    }
    if (shift >= nbit) {
      err = rerr_overflow;
      break;
    }
  }
  *resultp = v;
  *input = p;
  return err;
}

static rerr_t load_section(LPARAMS);
static rerr_t load_next_section(LPARAMS) {
  if (p >= end) return 0;
  MUSTTAIL return load_section(LARGS);
}

// skip_section skips the current section (or the remainder of the current section)
UNUSED static rerr_t skip_section(LPARAMS) {
  p = MIN(end, p + size);
  MUSTTAIL return load_next_section(LARGS);
}

static rerr_t load_section_DATA(LPARAMS) {
  u8 align_log2 = *p++; size--;
  if UNLIKELY(size == 0)
    return perr("DATA section ended prematurely");
  if UNLIKELY(align_log2 < 1 || align_log2 > 3)
    return perr("DATA section has invalid alignment (%u)", *(p-1));
  rom->dataalign = 1u << align_log2;
  rom->datasize = size;
  rom->data = p;
  p = MIN(end, p + size);
  MUSTTAIL return load_next_section(LARGS);
}

static rerr_t load_section_CODE(LPARAMS) {
  const void* code = (const void*)ALIGN2((uintptr)p, CODE_ALIGNMENT);
  usize codesize = size - (usize)(code - (const void*)p);
  if UNLIKELY(!IS_ALIGN2(codesize, CODE_ALIGNMENT))
    return perr("CODE section size %lu not aligned at %zu", codesize, CODE_ALIGNMENT);
  rom->code = code;
  rom->codelen = codesize / CODE_ALIGNMENT; // compiler replaces w/ shr; ie codesize>>2
  p += size;
  MUSTTAIL return load_next_section(LARGS);
}

static rerr_t load_section(LPARAMS) {
  assert(p < end);
  u8 kind = *p++;
  rerr_t err = leb_u64_read(&size, 64, &p, end);
  if UNLIKELY(err)
    return perr("invalid section 0x%02x header size: %s", kind, rerr_str(err));
  if UNLIKELY(size > (usize)(end - p))
    return perr("corrupt section 0x%02x", kind);
  switch (kind) {
    case RSM_ROM_DATA: MUSTTAIL return load_section_DATA(LARGS);
    case RSM_ROM_CODE: MUSTTAIL return load_section_CODE(LARGS);
    default:
      // TODO: consider 0x00 for "named custom sections", like WASM
      return perr("unknown section kind 0x%02x", kind);
  }
}

rerr_t rsm_loadrom(rrom_t* rom) {
  // default values
  rom->codelen   = 0;
  rom->datasize  = 0;
  rom->dataalign = 1;

  if UNLIKELY(rom->imgsize < 6 || *(u32*)rom->img->magic != RSM_ROM_MAGIC) {
    const void* p = rom->img; // for perr
    return perr("invalid ROM image");
  }

  if UNLIKELY(rom->img->version != 0) {
    const void* p = &rom->img->version; // for perr
    perr("unsupported ROM version %u", rom->img->version);
    return rerr_not_supported;
  }

  const u8* p = (const u8*)rom->img->data;
  const u8* end = p + rom->imgsize - sizeof(rromimg_t);
  if UNLIKELY(p == end)
    return 0;
  return load_section(rom, p, end, 0);
}


// --------------------------------------------------------------------------------------
// ROM builder
#ifndef RSM_NO_ASM

#define LEB_NBYTE_64 10  // number of bytes needed to represent all 64-bit integer values
#define LEB_NBYTE_32 5   // number of bytes needed to represent all 32-bit integer values


#define _LEB_MORE_S(T) \
  ( (tmp != 0 && tmp != (T)-1) || (val >= 0 && (byte&64)) || (val < 0 && (byte&64)==0) )
#define _LEB_MORE_U(T) \
  (tmp != 0)
#define _LEB_DEF_WRITE(NAME, T, NBYTE, MORE) \
  static usize NAME(u8 out[NBYTE], T val) {  \
    T tmp = val; bool more; usize i = 0;     \
    do {                                     \
      u8 byte = tmp & 127;                   \
      tmp >>= 7;                             \
      more = MORE(T);                        \
      if (more)                              \
        byte = byte | 128;                   \
      out[i++] = byte;                       \
    } while (more);                          \
    return i;                                \
  }

static usize leb_u64_write(u8 out[LEB_NBYTE_64], u64 val);
_LEB_DEF_WRITE(leb_u64_write, u64, LEB_NBYTE_64, _LEB_MORE_U)

// static usize leb_i64_write(u8 out[LEB_NBYTE_64], i64 val);
// _LEB_DEF_WRITE(leb_i64_write, i64, LEB_NBYTE_64, _LEB_MORE_S)

// static usize leb_u32_write(u8 out[LEB_NBYTE_32], u32 val);
// _LEB_DEF_WRITE(leb_u32_write, u32, LEB_NBYTE_32, _LEB_MORE_U)

// static usize leb_i32_write(u8 out[LEB_NBYTE_32], i32 val);
// _LEB_DEF_WRITE(leb_i32_write, i32, LEB_NBYTE_32, _LEB_MORE_S)


static void calc_DATA(rrombuild_t* rb, usize* bsize, usize* align) {
  if (rb->datasize)
    *bsize = 1 + rb->datasize; // 1+ for "align u8"
}
static u8* build_DATA(rrombuild_t* rb, rrom_t* rom, u8* p, rerr_t* errp) {
  assert(rb->dataalign != 0);
  assert(CEIL_POW2(rb->dataalign) == rb->dataalign);
  *p++ = rb->dataalign == 1 ? 1 : ILOG2(rb->dataalign);
  rom->data = p;
  rom->datasize = rb->datasize;
  rom->dataalign = rb->dataalign;
  rerr_t err = rb->filldata(p, rb->userdata);
  if (err)
    *errp = err;
  p += rb->datasize;
  return p;
}

static void calc_CODE(rrombuild_t* rb, usize* bsize, usize* align) {
  *bsize = rb->codelen * sizeof(rin_t);
  *align = CODE_ALIGNMENT;
}
static u8* build_CODE(rrombuild_t* rb, rrom_t* rom, u8* p, rerr_t* errp) {
  rom->code = (const rin_t*)p;
  rom->codelen = rb->codelen;
  memcpy(p, rb->code, rb->codelen*sizeof(rin_t));
  p += rb->codelen*sizeof(rin_t);
  return p;
}


rerr_t rom_build(rrombuild_t* rb, rmemalloc_t* ma, rrom_t* rom) {
  memset(rom, 0, sizeof(rrom_t)); // in case of error
  rom->dataalign = 1;

  // Calculate image size
  // Important: CALC_SECTION_SIZE order must match START_SECTION order
  usize imgz = sizeof(rromimg_t);
  usize sechsize[rrom_skind_MAX+1] = {0}; // header size with padding
  usize secbsize[rrom_skind_MAX+1] = {0}; // body size with padding (duplicate)
  usize secalign[rrom_skind_MAX+1] = {0};

  for (rrom_skind kind = rrom_skind_MIN; kind <= rrom_skind_MAX; kind++) {
    usize bsize = 0;
    usize align = 1;
    switch ((enum rrom_skind)kind) {
      case RSM_ROM_DATA: calc_DATA(rb, &bsize, &align); break;
      case RSM_ROM_CODE: calc_CODE(rb, &bsize, &align); break;
    }
    if (bsize == 0)
      continue;
    assert(align != 0);
    assert(CEIL_POW2(align) == align);
    u8 tmp[LEB_NBYTE_64];
    usize n = leb_u64_write(tmp, (u64)bsize + (u64)align-1);
    usize headerz = 1 + n; // kind u8 + size varint
    usize totalz = ALIGN2(imgz + headerz + bsize, align) - imgz;
    imgz += totalz;
    sechsize[kind] = totalz - bsize;
    secbsize[kind] = totalz - headerz;
    secalign[kind] = (usize)align;
    // usize padding = totalz - bsize - headerz;
    // dlog("[calc] section 0x%02x size: %zu (%zu + %zu + %zu) @%zu",
    //   kind, totalz, headerz, padding, bsize, align);
  }

  // allocate memory
  rmem_t imgmem = rmem_alloc_aligned(ma, imgz, sizeof(void*)); // TODO: alignment
  rromimg_t* img = imgmem.p;
  if UNLIKELY(img == NULL)
    return rerr_nomem;

  // set header
  *(u32*)img->magic = RSM_ROM_MAGIC; // "RSM\0"
  img->version = 0;

  // p points to current "fill location" of the image
  void* base = img;
  u8* p = base + sizeof(rromimg_t);

  for (rrom_skind kind = rrom_skind_MIN; kind <= rrom_skind_MAX; kind++) {
    usize bodysize = secbsize[kind];
    if (bodysize == 0)
      continue;
    usize headsize = sechsize[kind];

    // write section header
    dlog("section 0x%02x at %08lx: %zu B head", kind, (uintptr)((void*)p-base), headsize);
    #if DEBUG
    void* secstart = p; // used for assertion
    #endif
    *p++ = kind; headsize--;
    usize n = leb_u64_write(p, (u64)bodysize);
    p += n; headsize -= n;
    if (headsize) { // padding
      memset(p, 0, headsize);
      p += headsize;
      bodysize -= headsize; // remove padding (only for log message)
    }

    // write section body
    dlog("        body at %08lx: %zu B", (uintptr)((void*)p-base), bodysize);
    assertf(IS_ALIGN2((uintptr)((void*)p-base), secalign[kind]), "misaligned");
    rerr_t err = 0;
    switch ((enum rrom_skind)kind) {
      case RSM_ROM_DATA: p = build_DATA(rb, rom, p, &err); break;
      case RSM_ROM_CODE: p = build_CODE(rb, rom, p, &err); break;
    }
    if UNLIKELY(err) {
      rmem_free(ma, imgmem);
      return err;
    }

    // check that the actual size is what we expect (note that headsize == padding)
    assert((usize)((void*)p-secstart) == sechsize[kind]+secbsize[kind]-headsize);
  }

  // finalize rrom fields
  rom->img = img;
  rom->imgsize = (usize)((void*)p - base);
  rom->imgmem = imgmem;
  dlog("final ROM image size: %zu B", rom->imgsize);
  return 0;
}

#endif // RSM_NO_ASM
