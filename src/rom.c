// Implements ROM functions rsm_loadrom and rom_build
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#include "asm.h"

typedef u8 rrom_skind; // section kind
enum rrom_skind {
  RSM_ROM_CODE = 0x01,
  RSM_ROM_DATA = 0x02,

  rrom_skind_MIN = RSM_ROM_CODE,
  rrom_skind_MAX = RSM_ROM_DATA,
} RSM_END_ENUM(rrom_skind)

#define CODE_ALIGNMENT sizeof(rin_t) // alignment of CODE section body

UNUSED static const char* rrom_skind_name(rrom_skind kind) {
  switch ((enum rrom_skind)kind) {
    case RSM_ROM_CODE: return "CODE";
    case RSM_ROM_DATA: return "DATA";
  }
  return "?";
}

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
  p = (const void*)ALIGN2((uintptr)p, CODE_ALIGNMENT);
  if UNLIKELY(!IS_ALIGN2(size, CODE_ALIGNMENT))
    return perr("CODE section size %llu not aligned to %zu", size, CODE_ALIGNMENT);
  rom->code = (const void*)p;
  rom->codelen = (usize)( size / CODE_ALIGNMENT ); // #bytes -> #instructions
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

  if UNLIKELY(rom->imgsize < 6 || memcmp(&rom->img->magic, RSM_ROM_MAGIC, 4)) {
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

typedef struct {
  usize size;  // size of section body
  usize align; // alignment of section body
} secsize_t;

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

static usize leb_size(u64 val) {
  for (usize len = 0;;) {
    val >>= 7;
    len++;
    if (val == 0)
      return len;
  }
}


static void calc_DATA(rrombuild_t* rb, usize* bsize, usize* align) {
  if (rb->datasize == 0)
    return;
  // We add one byte for storage of data alignment value
  *bsize = rb->datasize + 1;
  *align = 1; // alignment in ROM image, not at runtime
}
static u8* build_DATA(rrombuild_t* rb, rrom_t* rom, u8* p, rerr_t* errp) {
  assert(rb->dataalign != 0);
  assert(IS_POW2(rb->dataalign));
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


static usize calc_rom_size(
  rrombuild_t* rb, rrom_t* rom, secsize_t* secsizev, usize* maxalignp)
{
  usize addr = sizeof(rromimg_t); // logical address; also effectively total size

  for (rrom_skind kind = rrom_skind_MIN; kind <= rrom_skind_MAX; kind++) {
    usize bodysize = 0;
    usize bodyalign = 1; // section body alignment in the ROM data
    switch ((enum rrom_skind)kind) {
      case RSM_ROM_CODE: calc_CODE(rb, &bodysize, &bodyalign); break;
      case RSM_ROM_DATA: calc_DATA(rb, &bodysize, &bodyalign); break;
    }
    if (bodysize == 0)
      continue;

    // #if DEBUG
    // usize startaddr = addr;
    // #endif

    // headsize = u8(kind) + len(LEB(bodysize))
    usize headsize = 1 + leb_size((u64)bodysize);
    addr += headsize;

    // align start of body
    assert(bodyalign > 0 && IS_POW2(bodyalign));
    addr = ALIGN2(addr, bodyalign);
    *maxalignp = MAX(*maxalignp, bodyalign);

    // bodysize
    secsizev[kind].size = bodysize;
    secsizev[kind].align = bodyalign;
    addr += bodysize;

    // dlog("[%s] %s section: %zu B = head(%zu) + padding(%zu) + body(%zu@%zu)",
    //   __FUNCTION__, rrom_skind_name(kind), addr - startaddr,
    //   headsize, ((addr - startaddr) - (headsize + bodysize)), bodysize, bodyalign);
  }

  return addr;
}


#if DEBUG
  static void dlog_section_layout(
    rromimg_t* img, secsize_t* secsizev, rrom_skind kind, uintptr secaddr, const u8* p)
  {
    uintptr headaddr = secaddr - (uintptr)img;
    uintptr bodyaddr = (uintptr)p - (uintptr)img;
    uintptr pad = ALIGN2((uintptr)p, secsizev[kind].align) - (uintptr)p;
    usize secsize = (usize)( ((uintptr)p + pad + secsizev[kind].size) - secaddr );
    const int kMaxSecNameLen = 4;
    dlog("  %*s section %08lx … %08lx (%zu B)", kMaxSecNameLen, rrom_skind_name(kind),
      secaddr - (uintptr)img, (secaddr - (uintptr)img) + secsize, secsize);
    dlog("  %*s  header %08lx (%zu B)", kMaxSecNameLen, "",
      headaddr, (uintptr)p - secaddr);
    if (pad) {
      dlog("  %*s padding %08lx (%zu B)", kMaxSecNameLen, "", bodyaddr, pad);
    }
    dlog("  %*s    body %08lx (%zu B, align %zu B)", kMaxSecNameLen, "",
      bodyaddr + pad, secsizev[kind].size, secsizev[kind].align);
  }
#endif


static rerr_t write_rom(
  rrombuild_t* rb, rrom_t* rom, secsize_t* secsizev, rromimg_t* img, usize* imgsizep)
{
  // ROM header
  memcpy(&img->magic, RSM_ROM_MAGIC, 4);
  img->version = 0;

  // p points to current "fill location" of the image
  u8* p = (u8*)img + sizeof(rromimg_t);

  #if DEBUG
  dlog("ROM image %08lx … %08lx (%zu B)", 0lu, (uintptr)*imgsizep, *imgsizep);
  #endif

  for (rrom_skind kind = rrom_skind_MIN; kind <= rrom_skind_MAX; kind++) {
    if (secsizev[kind].size == 0)
      continue;

    #if DEBUG
    uintptr secaddr = (uintptr)p; // used for assertion
    #endif

    // write header
    *p++ = kind;
    p += leb_u64_write(p, (u64)secsizev[kind].size);

    #if DEBUG
    dlog_section_layout(img, secsizev, kind, secaddr, p);
    #endif

    // align start of body
    p = (u8*)ALIGN2((uintptr)p, secsizev[kind].align);

    // write body
    rerr_t err = 0;
    switch ((enum rrom_skind)kind) {
      case RSM_ROM_DATA: p = build_DATA(rb, rom, p, &err); break;
      case RSM_ROM_CODE: p = build_CODE(rb, rom, p, &err); break;
    }
    if UNLIKELY(err) {
      *imgsizep = 0;
      return err;
    }
  }

  *imgsizep = (usize)( (uintptr)p - (uintptr)img );
  return 0;
}


rerr_t rom_build(rrombuild_t* rb, rmemalloc_t* ma, rrom_t* rom) {
  memset(rom, 0, sizeof(rrom_t)); // in case of error
  rom->dataalign = 1; // Note: this is set to rb->dataalign by build_DATA

  // calculate image size
  secsize_t secsizev[rrom_skind_MAX+1] = {0}; // size of each section
  usize maxalign = 1;
  rom->imgsize = calc_rom_size(rb, rom, secsizev, &maxalign);

  // allocate memory
  rmem_t imgmem = rmem_alloc_aligned(ma, rom->imgsize, maxalign);
  rromimg_t* img = imgmem.p;
  if UNLIKELY(img == NULL)
    return rerr_nomem;

  // write ROM image
  rerr_t err = write_rom(rb, rom, secsizev, img, &rom->imgsize);
  if UNLIKELY(err) {
    rmem_free(ma, imgmem);
    return err;
  }

  // finalize rrom fields
  rom->img = img;
  rom->imgmem = imgmem;
  return 0;
}

#endif // RSM_NO_ASM
