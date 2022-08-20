// Implements ROM functions rsm_loadrom and rom_build
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#include "asm.h"

// ROM version range this code can handle.
// rom_build produces version ROM_VERSION_MAX ROMs.
#define ROM_VERSION_MIN 0
#define ROM_VERSION_MAX 0

// RSM_WITH_LZ4: define to enable creation of LZ4 compressed ROMs
#define RSM_WITH_LZ4

// ROM_LOAD_TRACE: define to enable dlog of the loading process (in DEBUG mode only)
//#define ROM_LOAD_TRACE

#ifdef RSM_WITH_LZ4
  #include "lz4.h"
#endif

typedef u8 rrom_skind; // section kind
enum rrom_skind {
  RSM_ROM_CODE = 0x01,
  RSM_ROM_DATA = 0x02,

  rrom_skind_MIN = RSM_ROM_CODE,
  rrom_skind_MAX = RSM_ROM_DATA,
} RSM_END_ENUM(rrom_skind)

#define CODE_ALIGNMENT sizeof(rin_t) // alignment of CODE section body

#ifdef ROM_LOAD_TRACE
  #define traceload(args...) dlog(args)
#else
  #define traceload(args...) ((void)0)
#endif

UNUSED static const char* rrom_skind_name(rrom_skind kind) {
  switch ((enum rrom_skind)kind) {
    case RSM_ROM_CODE: return "CODE";
    case RSM_ROM_DATA: return "DATA";
  }
  return "?";
}

// --------------------------------------------------------------------------------------
// ROM loader

#define LPARAMS  rrom_t* rom, const u8* start, const u8* end, const u8* p, u64 size
#define LARGS    rom, start, end, p, size
#define POFFS    ( (usize)((const void*)p - (const void*)rom->img) )
#define perr(fmt, args...) ({ \
  log("%s" fmt " (at offset %zu)", errprefix, ##args, POFFS); \
  rerr_invalid; })


static const char* errprefix = "error while loading rom: ";


static rerr_t leb_u64_read(u64* resultp, u32 nbit, const u8** input, const void* inend) {
  rerr_t err = rerr_invalid;
  u64 v = 0;
  u32 shift = 0;
  const u8* p = *input;
  while (p != inend) {
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

// rrom_clear_pointers clears computed pointers into a ROMs data
static void rrom_clear_pointers(rrom_t* rom) {
  memset(&rom->datamem, 0, sizeof(rrom_t) - offsetof(rrom_t, datamem));
  rom->dataalign = 1;
}


usize rromimg_loadsize(const rromimg_t* img, usize imgsize) {
  if UNLIKELY(imgsize < sizeof(rromimg_t))
    return USIZE_MAX;

  if ((img->flags & RROM_LZ4) == 0) {
    uintptr dataaddr = (uintptr)img->data;
    usize adiff = (usize)( ALIGN2(dataaddr, CODE_ALIGNMENT) - dataaddr );
    return (imgsize - sizeof(rromimg_t)) + adiff;
  }

  if UNLIKELY(imgsize < sizeof(rromimg_t) + 1) // header + LEB128 int
    return USIZE_MAX;
  const u8* p = img->data;
  const u8* pend = (const u8*)img + imgsize;
  u64 uncompressed_size;
  rerr_t err = leb_u64_read(&uncompressed_size, 64, &p, pend);
  if UNLIKELY( err || (U64_MAX > USIZE_MAX && uncompressed_size > (u64)USIZE_MAX) )
    return USIZE_MAX;
  return (usize)uncompressed_size;
}


#ifdef RSM_WITH_LZ4
  static rerr_t rom_decompress(rrom_t* rom, rmem_t dstmem, usize* datasizep) {
    // A compressed ROM starts with the standard header:
    //   ┌────────────┬────────────┬──────────┐
    //   │ R  S  M 00 │ version u8 │ flags u8 │
    //   └────────────┴────────────┴──────────┘
    // followed by uncompressed size value and compressed data:
    //   ┌───────────────────────────┬───────────────────────┐
    //   │ uncompressed_size varuint │ compressed_data u8... │
    //   └───────────────────────────┴───────────────────────┘
    const char* src = (const char*)rom->img->data;
    const char* srcend = (const char*)rom->img + rom->imgsize;

    u64 uncompressed_size;
    rerr_t err = leb_u64_read(&uncompressed_size, 64, (const u8**)&src, srcend);
    if (err)
      return rerr_invalid;

    if ((u64)dstmem.size < uncompressed_size)
      return rerr_overflow;

    usize compressed_size = (usize)(uintptr)(srcend - src);

    // TODO: consider in-place decompression: usize inplace_dstsize =
    //   LZ4_DECOMPRESS_INPLACE_BUFFER_SIZE((usize)uncompressed_size);
    // if (rom->imgmemsize >= inplace_dstsize)
    //   dlog("TODO in-place decompress");

    // LZ4 API uses "int" for sizes, so check for overflow
    if UNLIKELY(__INT_MAX__ < USIZE_MAX && (
      dstmem.size > (usize)__INT_MAX__ || compressed_size > (usize)__INT_MAX__ ))
    {
      return rerr_overflow;
    }

    int z = LZ4_decompress_safe(src, dstmem.p, (int)compressed_size, (int)dstmem.size);
    if UNLIKELY(z < 0 || (u64)z != uncompressed_size) {
      dlog("LZ4_decompress_safe => %d (expected %llu)", z, uncompressed_size);
      log("%scorrupt compressed image", errprefix);
      return rerr_invalid;
    }

    *datasizep = uncompressed_size;
    return 0;
  }
#endif // RSM_WITH_LZ4


#define ALIGN_P_AND_CHECK_BOUNDS(alignment) { \
  p = (const void*)ALIGN2((uintptr)p, alignment); \
  if UNLIKELY(p >= end) \
    return perr("image ended prematurely"); \
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
  if UNLIKELY(align_log2 > ILOG2(RSM_ROM_ALIGN))
    return perr("DATA section has invalid alignment (%u)", *(p-1));
  rom->dataalign = 1u << align_log2;
  ALIGN_P_AND_CHECK_BOUNDS(rom->dataalign);
  rom->datasize = size;
  rom->data = p;
  p = MIN(end, p + size);
  MUSTTAIL return load_next_section(LARGS);
}


static rerr_t load_section_CODE(LPARAMS) {
  ALIGN_P_AND_CHECK_BOUNDS(CODE_ALIGNMENT);
  if UNLIKELY(!IS_ALIGN2(size, CODE_ALIGNMENT))
    return perr("CODE section size %llu not aligned to %zu", size, CODE_ALIGNMENT);
  rom->code = (const void*)p;
  rom->codelen = (usize)( size / CODE_ALIGNMENT ); // #bytes -> #instructions
  p += size;
  MUSTTAIL return load_next_section(LARGS);
}


static rerr_t load_section(LPARAMS) {
  assert(p < end);

  #ifdef ROM_LOAD_TRACE
  uintptr secoffs = (uintptr)(p - start);
  #endif

  u8 kind = *p++;
  rerr_t err = leb_u64_read(&size, 64, &p, end);
  if UNLIKELY(err)
    return perr("invalid section 0x%02x header size: %s", kind, rerr_str(err));

  if UNLIKELY(size > (usize)(end - p))
    return perr("corrupt section 0x%02x", kind);

  traceload("%s %s (0x%02x) at offset 0x%08lx (body: 0x%08lx %llu B)",
    __FUNCTION__, rrom_skind_name(kind), kind, secoffs, (uintptr)(p - start), size);

  switch (kind) {
    case RSM_ROM_DATA: MUSTTAIL return load_section_DATA(LARGS);
    case RSM_ROM_CODE: MUSTTAIL return load_section_CODE(LARGS);
    default:
      // TODO: consider 0x00 for "named custom sections", like WASM
      return perr("unknown section kind 0x%02x", kind);
  }
}


static rerr_t rsm_loadrom_direct(rrom_t* rom) {
  traceload("%s", __FUNCTION__);

  if (!IS_ALIGN2((uintptr)rom->img, RSM_ROM_ALIGN)) {
    log("%simage address %p not aligned to %u boundary",
      errprefix, rom->img, RSM_ROM_ALIGN);
    return rerr_invalid;
  }

  // p is our "read & decode" pointer
  const u8* p = rom->img->data;
  const u8* end = (void*)rom->img + rom->imgsize;
  if UNLIKELY(p >= end)
    return 0;

  return load_section(rom, p, end, p, 0);
}


static rerr_t rsm_loadrom_compressed(rrom_t* rom, rmem_t dst) {
#ifdef RSM_WITH_LZ4
  traceload("%s", __FUNCTION__);

  // copy body by decompressing it into dst.p.
  // Note: rom_decompress checks that dst is large enough for datasize.
  usize datasize;
  rerr_t err = rom_decompress(rom, dst, &datasize);
  if (err)
    return err;

  if UNLIKELY(datasize == 0)
    return 0;

  //writefile("uncompressed.rom", 0664, dst.p, datasize);

  return load_section(rom, dst.p, dst.p + datasize, dst.p, 0);

#else
  log("%scompression not supported", errprefix);
  return rerr_not_supported;
#endif
}


static rerr_t rsm_loadrom_uncompressed(rrom_t* rom, rmem_t dst) {
  traceload("%s", __FUNCTION__);

  usize datasize = rom->imgsize - sizeof(rromimg_t);
  if ((u64)dst.size < datasize) {
    log("%snot enough space at destination", errprefix);
    return rerr_overflow;
  }

  if UNLIKELY(datasize == 0)
    return 0;

  memcpy(dst.p, rom->img->data, datasize);

  return load_section(rom, dst.p, dst.p + datasize, dst.p, 0);
}


rerr_t rsm_loadrom(rrom_t* rom, rmem_t dst) {
  rrom_clear_pointers(rom);

  if UNLIKELY( rom->imgsize < sizeof(rromimg_t) ||
               memcmp(&rom->img->magic, RSM_ROM_MAGIC, 4) )
  {
    log("%sinvalid header", errprefix);
    return rerr_invalid;
  }

  u8 version = rom->img->version;
  if UNLIKELY(version < ROM_VERSION_MIN || version > ROM_VERSION_MAX) {
    const void* p = &rom->img->version; // for perr
    perr("unsupported version %u", rom->img->version);
    return rerr_not_supported;
  }

  if (dst.p == NULL) {
    if ((rom->img->flags & RROM_LZ4) == 0)
      return rsm_loadrom_direct(rom);
    log("%sdestination is NULL", errprefix);
    return rerr_invalid;
  }

  if UNLIKELY(!IS_ALIGN2((uintptr)dst.p, RSM_ROM_ALIGN)) {
    log("%sdestination address %p misaligned", errprefix, dst.p);
    return rerr_invalid;
  }

  if UNLIKELY(dst.size < sizeof(rromimg_t)) {
    log("%snot enough space at destination", errprefix);
    return rerr_overflow;
  }

  rom->datamem = dst;

  // copy header and clear compressed flag
  memcpy(dst.p, rom->img, sizeof(rromimg_t));
  ((rromimg_t*)dst.p)->flags &= ~RROM_LZ4;

  // advance dst pointer to body position
  dst.p += sizeof(rromimg_t);
  dst.size -= sizeof(rromimg_t);

  // copy body
  return (rom->img->flags & RROM_LZ4) ?
    rsm_loadrom_compressed(rom, dst) :
    rsm_loadrom_uncompressed(rom, dst);
}


void rsm_freerom(rrom_t* rom, rmemalloc_t* ma) {
  if (rom->imgmemsize == 0)
    return;
  rmem_free(ma, RMEM(rom->img, rom->imgmemsize));
  memset(rom, 0, sizeof(rrom_t));
}


// --------------------------------------------------------------------------------------
// ROM builder
#ifndef RSM_NO_ASM

typedef struct {
  usize size;  // size of section body
  usize align; // alignment of section body
} secsize_t;

// LEB128: Little Endian Base 128

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
  assert(rb->dataalign != 0);
  *bsize = rb->datasize + 1 + (rb->dataalign - 1);
  *align = 1;
}
static u8* build_DATA(rrombuild_t* rb, rrom_t* rom, u8* p, rerr_t* errp) {
  assert(rb->dataalign != 0);
  assert(IS_POW2(rb->dataalign));

  u8 align_log2 = (u8)ILOG2(rb->dataalign);
  *p++ = align_log2;

  u8* data = (u8*)ALIGN2((uintptr)p, rb->dataalign);
  rerr_t err = rb->filldata(data, rb->userdata);
  if (err)
    *errp = err;

  rom->data = data;
  rom->datasize = rb->datasize;
  rom->dataalign = rb->dataalign;

  p += rb->datasize + (rb->dataalign - 1);
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
  usize addr = sizeof(rromimg_t); // logical address; also effectively the total size

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

    // headsize = u8(kind) + len(LEB128(bodysize))
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
    rromimg_t* img, secsize_t* secsizev, rrom_skind kind, uintptr secaddr,
    const u8* p, const u8* p_body)
  {
    uintptr headaddr = secaddr - (uintptr)img;
    uintptr bodyaddr = (uintptr)p_body - (uintptr)img;
    uintptr pad = (uintptr)(p_body - p);

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
  rrombuild_t* rb,
  rrom_t*      rom,
  secsize_t*   secsizev,
  rromimg_t*   img,
  usize        imgsize)
{
  // ROM header
  memcpy(&img->magic, RSM_ROM_MAGIC, 4);
  img->version = ROM_VERSION_MAX;
  img->flags = 0;

  // p points to current "fill location" of the image
  u8* p = (u8*)img + sizeof(rromimg_t);
  uintptr align_offs = 0;

  dlog("ROM image %08lx … %08lx (%zu B)", 0lu, (uintptr)imgsize, imgsize);
  dlog("  header  %08lx … %08lx (%zu B)", 0lu, sizeof(rromimg_t), sizeof(rromimg_t));

  for (rrom_skind kind = rrom_skind_MIN; kind <= rrom_skind_MAX; kind++) {
    if (secsizev[kind].size == 0)
      continue;

    #if DEBUG
    uintptr secaddr = (uintptr)p; // used for assertion
    #endif

    // write header
    *p++ = kind;
    p += leb_u64_write(p, (u64)secsizev[kind].size);

    // align start of body
    u8* p_body =
      (u8*)(ALIGN2((uintptr)p - align_offs, secsizev[kind].align) + align_offs);
    #if DEBUG
    dlog_section_layout(img, secsizev, kind, secaddr, p, p_body);
    #endif
    p = p_body;

    // write body
    rerr_t err = 0;
    switch ((enum rrom_skind)kind) {
      case RSM_ROM_DATA: p = build_DATA(rb, rom, p, &err); break;
      case RSM_ROM_CODE: p = build_CODE(rb, rom, p, &err); break;
    }
    if UNLIKELY(err)
      return err;
  }

  // make sure we ended up with the expected size
  assertf((usize)(uintptr)(p - (u8*)img) == imgsize,
    "%zu != %zu", (usize)(uintptr)(p - (u8*)img), imgsize);

  return 0;
}


#ifdef RSM_WITH_LZ4
  static rerr_t rom_compress(rrom_t* rom, rmemalloc_t* ma) {
    // A compressed ROM starts with the standard header:
    //   ┌────────────┬────────────┬──────────┐
    //   │ R  S  M 00 │ version u8 │ flags u8 │
    //   └────────────┴────────────┴──────────┘
    // followed by uncompressed size value and compressed data:
    //   ┌───────────────────────────┬───────────────────────┐
    //   │ uncompressed_size varuint │ compressed_data u8... │
    //   └───────────────────────────┴───────────────────────┘
    assertf((rom->img->flags & RROM_LZ4) == 0, "already compressed");

    // uncompressed source data to compress
    u64 uncompressed_size = (u64)rom->imgsize - (u64)sizeof(rromimg_t);
    int srcsize = (int)uncompressed_size;
    const char* srcdata = (const char*)rom->img->data;

    // check for overflow (lz4 API uses "int" for memory sizes)
    if (__INT_MAX__ < U64_MAX && uncompressed_size > (u64)__INT_MAX__)
      return rerr_overflow;

    // size of compressed-data header
    usize cheadsize = leb_size(uncompressed_size);

    // calculate max size needed for compressed data
    int cdatacap = LZ4_COMPRESSBOUND(srcsize);

    // allocate memory for compressed image
    rmem_t imgmem = rmem_alloc(ma, sizeof(rromimg_t) + cheadsize + (usize)cdatacap);
    if (imgmem.p == NULL)
      return rerr_nomem;

    // copy header into new compressed image and set "compressed with LZ4" flag
    rromimg_t* compressed_img = imgmem.p;
    memcpy(compressed_img, rom->img, sizeof(rromimg_t));
    compressed_img->flags |= RROM_LZ4;

    // write compression header; a LEB128 int of the uncompressed size
    char* cdata = (char*)compressed_img->data;
    cdata += leb_u64_write((u8*)cdata, uncompressed_size);

    // actual memory allocated may be larger than requested
    usize cdatacap2 = imgmem.size - (sizeof(rromimg_t) + cheadsize);
    if (__INT_MAX__ >= USIZE_MAX || cdatacap2 <= (usize)__INT_MAX__)
      cdatacap = (int)cdatacap2;

    // LZ4_compress_default
    int compressed_size = LZ4_compress_default(srcdata, cdata, srcsize, cdatacap);
    if (compressed_size == 0) {
      dlog("LZ4_compress_default(src, dst, %d, %d) failed", srcsize, cdatacap);
      rmem_free(ma, imgmem);
      return rerr_overflow;
    }

    // new image size is header + compression header + compressed data
    usize imgsize = sizeof(rromimg_t) + cheadsize;
    if (check_add_overflow(imgsize, (usize)compressed_size, &imgsize)) {
      dlog("compressed_size=%d too large", compressed_size);
      rmem_free(ma, imgmem);
      return rerr_overflow;
    }

    // resize memory if it's a lot smaller than what's allocated
    // Note: if rmem_resize fails, it leaves the allocation unchanged
    if (imgmem.size - imgsize > 256)
      rmem_resize(ma, &imgmem, imgsize);

    dlog("compressed image %zu B -> %zu B (%zux, %.1f%%)", rom->imgsize, imgsize,
      rom->imgsize / imgsize, ((double)imgsize / (double)rom->imgsize) * 100.0);

    // replace ROM image
    if (rom->imgmemsize > 0)
      rmem_free(ma, RMEM(rom->img, rom->imgmemsize));
    memset(rom, 0, sizeof(rrom_t));
    rom->img = imgmem.p;
    rom->imgsize = imgsize;
    rom->imgmemsize = imgmem.size;

    return 0;
  }
#endif // RSM_WITH_LZ4


rerr_t rom_build(rrombuild_t* rb, rmemalloc_t* ma, rrom_t* rom) {
  memset(rom, 0, sizeof(rrom_t));
  rom->dataalign = 1; // Note: this is set to rb->dataalign by build_DATA

  // calculate image size
  secsize_t secsizev[rrom_skind_MAX+1] = {0}; // size of each section
  usize maxalign = 1;
  rom->imgsize = calc_rom_size(rb, rom, secsizev, &maxalign);

  // allocate memory
  rmem_t imgmem = rmem_alloc_aligned(ma, rom->imgsize, RSM_ROM_ALIGN);
  if UNLIKELY(imgmem.p == NULL)
    return rerr_nomem;

  // write ROM image
  rromimg_t* img = imgmem.p;
  rerr_t err = write_rom(rb, rom, secsizev, img, rom->imgsize);
  if UNLIKELY(err)
    goto error;
  rom->img = img;
  rom->imgmemsize = imgmem.size;

  //writefile("original.rom", 0664, rom->img->data, rom->imgsize - sizeof(rromimg_t));

  // compress ROM data if requested
  #ifdef RSM_WITH_LZ4
    if ((rb->flags & RASM_NOCOMPRESS) == 0) {
      if ((err = rom_compress(rom, ma)))
        goto error;
    }
  #endif

  dlog("final rom image size: %zu B", rom->imgsize);
  return 0;
error:
  rmem_free(ma, imgmem);
  memset(rom, 0, sizeof(rrom_t));
  return err;
}

#endif // RSM_NO_ASM
