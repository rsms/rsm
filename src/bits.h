// operations on bits
// SPDX-License-Identifier: Apache-2.0

RSM_ASSUME_NONNULL_BEGIN


inline static bool bit_get(const void* bits, usize bit) {
  return ((u8*)bits)[bit / 8] & (1lu << (bit % 8));
}

inline static void bit_set(void* bits, usize bit) {
  ((u8*)bits)[bit / 8] |= (1 << (bit % 8));
}

inline static void bit_clear(void* bits, usize bit) {
  ((u8*)bits)[bit / 8] &= ~(1 << (bit % 8));
}

void bits_set_range(u8* bits, usize start, usize len, bool on);

// set of bits
typedef struct {
  u8*   data;
  usize len; // number of bits at data
} bitset_t;


inline static void bitset_init(bitset_t* bset, u8* data, usize len) {
  bset->data = data;
  bset->len = len;
}

inline static bool bitset_get(const bitset_t* bset, usize index) {
  assert(index < bset->len);
  return bit_get(bset->data, index);
}

inline static void bitset_set_range(bitset_t* bset, usize start, usize len, bool on) {
  assert(start + len <= bset->len);
  bits_set_range(bset->data, start, len, on);
}

// bitset_find_unset_range searches for a contiguous region of unset bits.
//   start: #bit to start the search at.
//   minlen: minimum number of unset bytes needed
//   maxlen: maximum number of unset bytes to consider
// Returns >=minlen if a range was found (and updates startp.)
// Returns 0 if no range large enough was found (may still update startp.)
usize bitset_find_unset_range(
  bitset_t* bset, usize* startp, usize minlen, usize maxlen);

// bitset_find_best_fit searches for the smallest hole that is >=minlen large
usize bitset_find_best_fit(bitset_t* bset, usize* startp, usize minlen);

inline static usize bitset_find_first_fit(bitset_t* bset, usize* startp, usize minlen) {
  return bitset_find_unset_range(bset, startp, minlen, minlen);
}

RSM_ASSUME_NONNULL_END
