// hash functions and PRNG
// SPDX-License-Identifier: Apache-2.0
#pragma once
RSM_ASSUME_NONNULL_BEGIN

// fastrand updates the PRNG and returns the next "random" number
u32 fastrand();
void fastrand_seed(u64 seed); // (re)sets the seed of fastrand

// hash_t is the storage type for hash functions
#if defined(__wasm__)
  typedef u64 hash_t;
  #define HASHCODE_MAX U64_MAX
#else
  typedef usize hash_t;
  #define HASHCODE_MAX USIZE_MAX
#endif

// hash computes a hash code for data p of size bytes length
static hash_t hash(const void* p, usize size, hash_t seed);
hash_t hash_2(const void* p, hash_t seed); // 2 bytes (eg. i16, u16)
hash_t hash_4(const void* p, hash_t seed); // 4 bytes (eg. i32, u32)
hash_t hash_8(const void* p, hash_t seed); // 8 bytes (eg. i64, u64)
hash_t hash_f32(const f32* p, hash_t seed); // f32, supports ±0 and NaN
hash_t hash_f64(const f64* p, hash_t seed); // f64, supports ±0 and NaN
hash_t hash_mem(const void* p, usize size, hash_t seed); // size bytes at p
inline static hash_t hash(const void* p, usize size, hash_t seed) {
  switch (size) {
    case 2:  return hash_2(p, seed);
    case 4:  return hash_4(p, seed);
    case 8:  return hash_8(p, seed);
    default: return hash_mem(p, size, seed);
  }
}

// uintptr hash_ptr(const void* p, uintptr seed)
// Must be a macro rather than inline function so that we can take its address.
#if UINTPTR_MAX >= 0xFFFFFFFFFFFFFFFFu
  #define hash_ptr hash_8
#else
  #define hash_ptr hash_4
#endif

RSM_ASSUME_NONNULL_END
