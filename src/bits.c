// bit set
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#include "bits.h"

static const u8 kBitsetMaskFirst[8] = { 0xFF, 0xFE, 0xFC, 0xF8, 0xF0, 0xE0, 0xC0, 0x80 };
static const u8 kBitsetMaskLast[8] = { 0x00, 0x1, 0x3, 0x7, 0xF, 0x1F, 0x3F, 0x7F };


void bitset_set_range(bitset_t* bset, usize start, usize len, bool on) {
  assert(len > 0);
  assert(start+len <= bset->len);

  u8* first = &bset->data[start / 8];
  u8* last = &bset->data[(start + len) / 8];
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
  bitset_t* bset, usize* startp, usize minlen, usize maxlen)
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

  assert(maxlen >= minlen);

  // We'll work with a register sized granule ("bucket") over the bitset
  usize bucket_bits = 8 * sizeof(usize); // bit size of one "bucket"
  usize* buckets = (usize*)bset->data;

  const usize start_bucket = (usize)(uintptr)*startp / bucket_bits;
  const usize end_bucket = bset->len / bucket_bits; // bucket index to stop before
  //dlog("start_bucket startp=%zu => %zu", *startp, start_bucket);

  u8 start_bitoffs = (u8)(*startp % bucket_bits); // bit index to start at

  usize freelen = 0; // current "free range" length

  for (usize bucket = start_bucket; bucket < end_bucket; bucket++) {
    //dlog("** bucket %zu %zx", bucket, buckets[bucket]);

    // if bucket is full
    if (buckets[bucket] == USIZE_MAX) {
      if (freelen >= minlen)
        goto found;
      freelen = 0; // reset start of free range
      start_bitoffs = 0; // reset start bit
      continue;
    }

    // if bucket is empty
    if (buckets[bucket] == 0lu) {
      if (freelen == 0)
        *startp = bucket * bucket_bits;
      freelen += bucket_bits - start_bitoffs;
      if (freelen >= maxlen) {
        //dlog("-> %zu", maxlen);
        *startp += start_bitoffs;
        return maxlen;
      }
      start_bitoffs = 0; // reset start bit
      continue;
    }

    // else: bucket has some free space; scan its bits
    usize bucket_val = buckets[bucket];
    u8 nbits = start_bitoffs; // number of bits we've looked at
    u32 tz = 0; // number of trailing zeroes
    bucket_val >>= nbits;
    start_bitoffs = 0;

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
  //dlog("-> MIN(%zu, %zu) = %zu", freelen, maxlen, MIN(freelen, maxlen));
  return MIN(freelen, maxlen);
}


// bitset_find_best_fit searches for the smallest hole that is >=minlen large
usize bitset_find_best_fit(bitset_t* bset, usize* startp, usize minlen) {
  usize start = *startp;
  usize best_start = 0;
  usize best_len = bset->len + 1;
  usize found = 0;
  //dlog("bitset_find_best_fit minlen=%zu", minlen);
  for (;;) {
    usize rangelen = bitset_find_unset_range(bset, &start, minlen, best_len);
    //dlog(">> bitset_find_unset_range => start=%zu len=%zu", start, rangelen);
    if (best_len > rangelen || !found) {
      if (!rangelen)
        break;
      //dlog(">> new best range found: start=%zu len=%zu", start, rangelen);
      best_start = start;
      best_len = rangelen;
      found = USIZE_MAX;
    }
    start += rangelen;
  }
  *startp = best_start & found;
  return minlen & found;
}
