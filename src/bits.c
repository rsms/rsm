// bit set
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#include "bits.h"

// BITS_TRACE: uncomment to enable logging a lot of info via dlog
//#define BITS_TRACE

// BITS_RUN_TEST_ON_INIT: uncomment to run tests during exe init in DEBUG builds
#define BITS_RUN_TEST_ON_INIT

#if defined(BITS_TRACE) && defined(DEBUG)
  #define trace(fmt, args...) dlog("[bits] " fmt, ##args)
#else
  #ifdef BITS_TRACE
    #warning BITS_TRACE has no effect unless DEBUG is enabled
    #undef BITS_TRACE
  #endif
  #define trace(...) ((void)0)
#endif


static const u8 kBitsetMaskFirst[8] = { 0xFF, 0xFE, 0xFC, 0xF8, 0xF0, 0xE0, 0xC0, 0x80 };
static const u8 kBitsetMaskLast[8] = { 0x00, 0x1, 0x3, 0x7, 0xF, 0x1F, 0x3F, 0x7F };


#ifdef BITS_TRACE
  static const char* debug_fmt_bits(const void* bits, usize len) {
    static char bufs[2][128];
    static int nbuf = 0;
    char* buf = bufs[nbuf++ % 2];
    assert(len <= sizeof(bufs[0]) - 1);
    usize i = 0;
    for (; i < len; i++)
      buf[i] = '0' + bit_get(bits, i);
    buf[i] = 0;
    return buf;
  }
#endif


void bits_set_range(u8* bits, usize start, usize len, bool on) {
  assert(len > 0);

  #if 0
  if (on) {
    for (usize end = start + len; start < end; start++)
      bit_set(bits, start);
  } else {
    for (usize end = start + len; start < end; start++)
      bit_clear(bits, start);
  }
  #else

  u8* first = &bits[start / 8];
  u8* last = &bits[(start + len) / 8];
  u8 mask = kBitsetMaskFirst[start % 8];

  if (first == last) {
    mask &= kBitsetMaskLast[(start + len) % 8];
    // branchless (*first = on ? (*first | mask) : (*first & ~mask))
    *first = COND_BYTE_MASK(*first, mask, on);
    return;
  }

  *first = COND_BYTE_MASK(*first, mask, on);

  mask = kBitsetMaskLast[(start + len) % 8];
  *last = COND_BYTE_MASK(*last, mask, on);

  first++;

  // set all bytes in between
  memset(first, 0xFF * (u8)on, last - first);
  #endif
}


usize bitset_find_unset_range(
  bitset_t* bset, usize* startp, usize minlen, usize maxlen, usize stride)
{
  // First fit implementation
  //
  //           0   1   2   3   4   5   6   7   8   9  10  11  12  13  14
  //         ┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
  // bit     │ ╳ │ ╳ │ ╳ │ ╳ │   │   │ ╳ │   │   │   │   │   │ ╳ │   │   │
  //         ├───┴───┴───┴───┼───┴───┴───┴───┼───┴───┴───┴───┼───┴───┴───┘
  // bucket  │       ╳       │       /       │               │ trailing bits
  //         └───────────────┴───────────────┴───────────────┘
  //                 1               2               3
  //           ↑
  //         start
  //
  // We start by splitting up the bitmask into register-sized (bucket_bits) buckets.
  // Then we iterate over the buckets in order. If a bucket is MAXSIZE we know there
  // are no free bits. If a bucket is 0 we know there are bucket_bits free bits.
  // If a bucket is neither full nor empty, we scan its individual bits for a free
  // range. Finally, if bset->len-start is not aligned to bucket_bits, we scan the
  // "trailing bits" for a free range.

  assert(stride > 0);
  assert(minlen > 0);
  assert(maxlen >= minlen);

  trace("start         %4zu", *startp);
  trace("min-maxlen    %4zu–%zu", minlen, maxlen);
  trace("stride        %4zu", stride);

  // We'll work at a register-sized granule ("bucket") over the bitset
  const usize bucket_bits = 8 * sizeof(usize); // bit size of one "bucket"
  usize bucket_stride = IDIV_CEIL(stride, bucket_bits);
  trace("bucket_bits   %4zu", bucket_bits);
  trace("bucket_stride %4zu", bucket_stride);

  const usize start_bucket = (usize)(uintptr)*startp / bucket_bits;
  // const usize start_bucket = (usize)(uintptr)IDIV_CEIL(*startp, bucket_bits);
  const usize end_bucket = (bset->len / bucket_bits) + 1;
  trace("end_bucket    %4zu (bset.len %zu)", end_bucket, bset->len);
  trace("start_bucket  %4zu (startp %zu)", start_bucket, *startp);

  // bit index to start at
  usize start_bit = *startp % bucket_bits;
  trace("start_bit     %4zu", start_bit);

  // bitwise stride
  usize stride2 = stride % bucket_bits; // remainder after "removing" bucket_stride
  stride2 += stride * (stride2 == 0); // if (stride2 == 0) stride2 = stride
  stride = stride2;
  trace("stride        %4zu (remainder)", stride);

  usize freelen = 0; // current "free range" length
  usize* buckets = (usize*)bset->data;
  usize bucket = start_bucket;

  for (; bucket < end_bucket; ) {

    // if bucket is full
    if (buckets[bucket] == USIZE_MAX) {
      trace("** buckets[%zu] = %zx  FULL", bucket, buckets[bucket]);
      if (freelen >= minlen)
        goto found;
      freelen = 0; // reset start of free range
      start_bit = 0; // reset start bit
      bucket = ALIGN_CEIL(bucket + 1, bucket_stride);
      continue;
    }

    // if bucket is empty
    if (buckets[bucket] == 0lu) {
      trace("** buckets[%zu] = %zx  EMPTY", bucket, buckets[bucket]);
      if (freelen == 0) {
        *startp = (bucket * bucket_bits) + start_bit;
        trace("   set startp = %zu", *startp);
      }
      freelen += bucket_bits - start_bit;
      trace("   freelen %zu", freelen);
      if (freelen >= maxlen) {
        trace("-> %zu…%zu (%zu = maxlen)", *startp, *startp + maxlen, maxlen);
        return maxlen;
      }
      start_bit = 0;
      bucket++;
      continue;
    }

    // else: bucket has some free space; scan its bits
    trace("** buckets[%zu] = %zx  PARTIAL", bucket, buckets[bucket]);

    usize bucket_val = buckets[bucket];
    usize nbits = start_bit; // number of bits we've looked at
    bucket_val >>= nbits;    // remove bits we've looked at from bucket_val

    while (nbits < bucket_bits) {
      trace("  nbits %zu, freelen %zu, bucket_val:\n          %s",
        nbits, freelen, debug_fmt_bits(&bucket_val, sizeof(bucket_val)*8 - nbits));

      if (freelen == 0) {
        usize start = (bucket * bucket_bits) + nbits;
        usize start_aligned = ALIGN_CEIL(start, stride);
        nbits += start_aligned - start;
        *startp = start_aligned;
        trace("    set start %zu", *startp);
        assertf(*startp == ALIGN_CEIL(*startp, stride), "%zu != %zu",
          *startp, ALIGN_CEIL(*startp, stride));
      }

      if (bucket_val == 0) {
        // rest of bucket is free
        trace("    bucket_val = 0");
        freelen += bucket_bits - nbits;
        nbits = bucket_bits; // end the while loop
        continue;
      }

      // count trailing zeroes (i.e. "free bits")
      //   e.g. bucket_val = 0b11000
      //   rsm_ctz(bucket_val) = 3
      //   bucket_val >> 3 = 0b11
      u32 trailing_zeroes = rsm_ctz(bucket_val);
      u32 tzrem = trailing_zeroes % stride;
      trace("    trailing_zeroes %u (%u + tzrem %u)",
        trailing_zeroes, trailing_zeroes - tzrem, tzrem);
      trailing_zeroes -= tzrem;
      bucket_val >>= trailing_zeroes;
      freelen += trailing_zeroes;
      nbits += trailing_zeroes;

      if (freelen >= minlen)
        goto found;

      // if stride caused us to not count all trailing zeroes,
      // skip the ones we didn't skip already now, before skipping trailing ones.
      bucket_val >>= tzrem;

      // Skip trailing ones and reset freelen
      //   e.g. bucket_val = 0b110000111
      //   rsm_ctz(bucket_val) = 3
      //   bucket_val >> 3 = 0b110000
      u32 trailing_ones = rsm_ctz(~bucket_val);
      u32 nbits_remaining = (u32)sizeof(bucket_val)*8 - nbits;
      trailing_ones = ALIGN_CEIL(trailing_ones, MIN(stride, nbits_remaining - 1));
      bucket_val >>= trailing_ones;
      nbits += trailing_ones;
      freelen = 0;
    }

    // The partial bucket did not have enough unset bits.
    // Loop.
    start_bit = 0;
    bucket = ALIGN_CEIL(bucket + 1, bucket_stride);
  }

  if (freelen >= minlen)
    goto found;

  trace("** scan tail bits");
  usize ftb = (bset->len / bucket_bits) * bucket_bits; // first trailing #bit
  usize trailing_bits = bset->len % bucket_bits;
  for (usize i = 0; i < trailing_bits; ++i) {
    if (bitset_get(bset, ftb + i)) {
      freelen = 0; // reset freelen
      continue;
    }
    // bit is not set; this slot is free
    if (freelen == 0)
      *startp = ftb + i;
    freelen++;
    if (freelen >= minlen)
      goto found;
  }
  trace("   not enough consecutive bits set");

  // no free range found that satisfies freelen >= minlen
  freelen = 0;
found:
  trace("-> %zu…%zu (%zu = MIN(freelen(%zu), maxlen(%zu)))",
    *startp, *startp + MIN(freelen, maxlen), MIN(freelen, maxlen), freelen, maxlen);
  return MIN(freelen, maxlen);
}


// bitset_find_best_fit searches for the smallest hole that is >=minlen large
usize bitset_find_best_fit(bitset_t* bset, usize* startp, usize minlen, usize stride) {
  usize start = *startp;
  usize best_start = 0;
  usize best_len = bset->len + 1;
  usize found = 0;
  for (;;) {
    usize rangelen = bitset_find_unset_range(bset, &start, minlen, best_len, stride);
    //dlog(">> bitset_find_unset_range => start=%zu len=%zu", start, rangelen);
    if (best_len > rangelen || !found) {
      if (!rangelen)
        break;
      //dlog(">> new best range found: start=%zu len=%zu", start, rangelen);
      best_start = start;
      best_len = rangelen;
      found = USIZE_MAX;
    }
    // TODO: this needs more testing.
    // Is stride correctly calculated here? Do we need to include *startp?
    start += ALIGN_CEIL(rangelen, stride);
  }
  *startp = best_start & found;
  return minlen & found;
}


#if defined(BITS_RUN_TEST_ON_INIT) && defined(DEBUG)
__attribute__((constructor)) static void test_bits() {
  dlog("test_bits");
  void* data[128]; // = 8192 bits
  memset(data, 0, sizeof(data));
  bitset_t bset = { .data = (u8*)data, .len = sizeof(data)*8 };

  usize region_minlen = 8;
  usize region_maxlen = region_minlen;
  usize stride = 1;
  usize region_start = 0; // bit to start searching
  usize region_len; // number of bits found

  // since all bits are 0, we should immediately find a range
  region_len = bitset_find_unset_range(
    &bset, &region_start, region_minlen, region_maxlen, stride);
  assert(region_start == 0);
  assert(region_len == region_minlen);

  // since all bits are 0, we should immediately find a range (stride differs)
  stride = 32;
  region_start = stride - 1;
  region_len = bitset_find_unset_range(
    &bset, &region_start, region_minlen, region_maxlen, stride);
  assert(region_len == region_minlen);
  assert(region_start == stride - 1);

  stride = 1024;
  region_start = stride - 1;
  region_minlen = region_maxlen = 100;
  region_len = bitset_find_unset_range(
    &bset, &region_start, region_minlen, region_maxlen, stride);
  assertf(region_len == region_minlen, "region_len=%zu", region_len);
  assertf(region_start == stride - 1, "region_start=%zu", region_start);

  // bug 1: stride fails when first three bits are set (does not respect stride)
  bitset_set_range(&bset, 0, 3, true);
  stride = 8;
  region_start = 0;
  region_minlen = region_maxlen = 2;
  region_len = bitset_find_unset_range(
    &bset, &region_start, region_minlen, region_maxlen, stride);
  assertf(region_len == region_minlen, "region_len=%zu", region_len);
  assertf(region_start == stride, "region_start=%zu", region_start);

  // bug 2
  bitset_set_range(&bset, 0, 3, true);
  stride = 1024;
  region_start = 0;
  region_minlen = region_maxlen = 1024;
  region_len = bitset_find_unset_range(
    &bset, &region_start, region_minlen, region_maxlen, stride);
  assertf(region_len == region_minlen,
    "region_len (%zu) != %zu", region_len, region_minlen);
  assertf(region_start == stride,
    "region_start (%zu) != %zu", region_start, stride);
}
#endif

