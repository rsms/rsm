// smap is a byte string to pointer map, implemented as a hash map
// SPDX-License-Identifier: Apache-2.0
#pragma once
#include "hash.h"
RSM_ASSUME_NONNULL_BEGIN

typedef struct smap    smap;    // string-keyed map
typedef struct smapent smapent; // smap entry
typedef u8             maplf;   // load factor
struct smapent {
  const char* nullable key; // NULL if this entry is empty
  usize                keylen;
  uintptr              value;
};
struct smap {
  u32          cap;  // capacity of entries
  u32          len;  // number of items currently stored in the map (count)
  u32          gcap; // growth watermark cap
  maplf        lf;   // growth watermark load factor (shift value; 1|2|3|4)
  hash_t       hash0; // hash seed
  union {
    smapent* entries;
    rmem_t   entries_mem;
  };
  rmemalloc_t* memalloc;
};
enum maplf {
  MAPLF_1 = 1, // grow when 50% full; recommended for maps w/ balanced hit & miss lookups
  MAPLF_2 = 2, // grow when 75% full; recommended for maps of mostly hit lookups
  MAPLF_3 = 3, // grow when 88% full; miss (no match) lookups are expensive
  MAPLF_4 = 4, // grow when 94% full; miss lookups are very expensive
} RSM_END_ENUM(maplf)

// smap_make initializes a new map m.
// hint can be 0 and provides a hint as to how many items will initially be stored.
// Returns m on success, NULL on memory allocation failure or overflow from large hint.
smap* nullable smap_make(smap* m, rmemalloc_t*, u32 hint, maplf);

// smap_copy creates a copy of srcm at dstm, using entries_mem.
// Assumes entries_mem is owned by ma; any modifications to the map will cause
// entries_mem to be passed to the ma.
// If there's not enough room in entries_mem, the copy is not completed.
// Returns the number of bytes needed for a copy as if entries_mem.size was infinite.
usize smap_copy(smap* dstm, const smap* srcm, rmem_t entries_mem, rmemalloc_t* ma);

// smap_dispose frees m->entries. m is invalid (use smap_make to reuse m)
void smap_dispose(smap* m);

// smap_clear removes all items. m remains valid
void smap_clear(smap* m);

// smap_assign assigns to the map, returning the location for its value,
// or NULL if memory allocation during growth failed. May return an existing item's value.
uintptr* nullable smap_assign(smap* m, const char* key, usize keylen);

// smap_lookup retrieves the value for key; NULL if not found.
uintptr* nullable smap_lookup(const smap* m, const char* key, usize keylen);

// smap_del removes an entry for key, returning whether an entry was deleted or not
bool smap_del(smap* m, const char* key, usize keylen);

// smap_itstart and smap_itnext iterates over a map.
// You can change the value of an entry during iteration but must not change the key.
// Any mutation to the map during iteration will invalidate the iterator.
// Example use:
//   for (smapent* e = smap_itstart(m); smap_itnext(m, &e); )
//     log("%.*s => %lx", (int)e->keylen, e->key, e->value);
inline static const smapent* nullable smap_itstart(const smap* m) { return m->entries; }
bool smap_itnext(const smap* m, const smapent** ep);

// smap_optimize tries to improve the key distribution of m by trying different
// hash seeds. Returns the best smap_score or <0.0 if rmem_alloc(ma) failed,
// leaving m with at least as good key distribution as before the call.
// ma is used to allocate temporary space for m's entries;
// space needed is m->entries*sizeof(smapent).
double smap_optimize(smap* m, usize iterations, rmemalloc_t* ma);

// smap_cfmt prints C code for a constant static representation of m
usize smap_cfmt(char* buf, usize bufcap, const smap* m, const char* name);

RSM_ASSUME_NONNULL_END
