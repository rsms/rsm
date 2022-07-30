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


void bits_set_range(u8* bits, usize start, usize len, bool on) {
  assert(len > 0);

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

  // We'll work at a register-sized granule ("bucket") over the bitset
  const usize bucket_bits = 8 * sizeof(usize); // bit size of one "bucket"
  usize bucket_stride = IDIV_CEIL(stride, bucket_bits);
  trace("bucket_bits   %4zu", bucket_bits);
  trace("bucket_stride %4zu", bucket_stride);

  const usize start_bucket = (usize)(uintptr)*startp / bucket_bits;
  // const usize start_bucket = (usize)(uintptr)IDIV_CEIL(*startp, bucket_bits);
  const usize end_bucket = bset->len / bucket_bits;
  trace("end_bucket    %4zu (bset.len %zu)", end_bucket, bset->len);
  trace("start_bucket  %4zu (startp %zu)", start_bucket, *startp);

  u8 start_bit = (u8)(*startp % bucket_bits); // bit index to start at
  trace("start_bit     %4u", start_bit);

  usize freelen = 0; // current "free range" length
  usize* buckets = (usize*)bset->data;

  for (usize bucket = start_bucket; bucket < end_bucket; bucket += bucket_stride) {


    // if bucket is full
    if (buckets[bucket] == USIZE_MAX) {
      trace("** buckets[%zu] = %zx  FULL", bucket, buckets[bucket]);
      if (freelen >= minlen)
        goto found;
      freelen = 0; // reset start of free range
      start_bit = 0; // reset start bit
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
        // *startp += start_bit;
        // trace("     set startp = %zu", *startp);
        trace("   -> %zu", maxlen);
        return maxlen;
      }
      start_bit = 0;
      continue;
    }

    // else: bucket has some free space; scan its bits
    trace("** buckets[%zu] = %zx  PARTIAL", bucket, buckets[bucket]);
    usize bucket_val = buckets[bucket];
    u8 nbits = start_bit; // number of bits we've looked at
    u32 tz = 0; // number of trailing zeroes
    bucket_val >>= nbits;
    start_bit = 0;

    // visit each bit
    while (nbits < bucket_bits) {
      if (bucket_val == 0) {
        if (freelen == 0)
          *startp = bucket * bucket_bits + nbits;
        freelen += bucket_bits - nbits;
        nbits = bucket_bits;
      } else {
        tz = rsm_ctz(bucket_val);
        bucket_val >>= tz;

        if (freelen == 0)
          *startp = bucket * bucket_bits + nbits;
        freelen += tz;
        nbits += tz;

        if (freelen >= minlen)
          goto found;

        // Deleting trailing ones.
        u32 trailing_ones = rsm_ctz(~bucket_val);
        bucket_val >>= trailing_ones;
        nbits += trailing_ones;
        freelen = 0;
      }
    }
  }

  if (freelen >= minlen)
    goto found;

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

  // no free range found that satisfies freelen >= minlen
  freelen = 0;
found:
  trace("-> %zu (= MIN(%zu, %zu))", MIN(freelen, maxlen), freelen, maxlen);
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

  // stride = 1024;
  // region_start = 114687;
  // region_len = bitset_find_unset_range(
  //   &bset, &region_start, region_minlen, region_maxlen, stride);
  // assertf(region_len == region_minlen, "region_len=%zu", region_len);
  // assertf(region_start == stride - 1, "region_start=%zu", region_start);
}
#endif

