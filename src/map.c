// compact hash map
// SPDX-License-Identifier: Apache-2.0
//
// Implemented with open addressing and linear probing.
// This code has been written, tested and tuned for a balance between small (mc) code size,
// a clear and simple implementation and lastly performance. The entire implementation is
// about 300 x86_64 instructions (20 branches), 270 arm64 instructions (35 branches.)
//
#include "rsmimpl.h"
#include "map.h"
#include "abuf.h"

#define DELMARK ((const char*)1) /* assume no key is ever at address 0x1 */

inline static bool streq(const smapent* ent, const char* key, usize keylen) {
  return ent->keylen == keylen && memcmp(ent->key, key, keylen) == 0;
}

// captab maps MAPLF_1...4 to multipliers for cap -> gcap
static const double captab[] = { 0.5, 0.25, 0.125, 0.0625, 0.0 };

static u32 perfectcap(u32 len, maplf lf) {
  assert(lf > 0 && lf <= countof(captab));
  len++; // must always have one free slot
  return CEIL_POW2(len + (u32)( (double)len*captab[lf-1] + 0.5 ));
}

static smap* smap_init(
  smap* m, rmemalloc_t* ma, rmem_t entries_mem, u32 cap, u32 hash0, maplf lf)
{
  assert(entries_mem.p && entries_mem.size);
  assert(IS_ALIGN2((uintptr)entries_mem.p, _Alignof(smapent)));
  assert(entries_mem.size >= cap * sizeof(smapent));
  m->memalloc = ma;
  m->len = 0;
  m->cap = cap;
  // lf is a bit shift magnitude that does fast integer division
  // i.e. cap-(cap>>lf) == (u32)((double)cap*0.75)
  m->lf = lf;
  m->gcap = cap - (cap >> lf);
  m->hash0 = hash0;
  m->entries_mem = entries_mem;
  return m;
}

smap* nullable smap_make(smap* m, rmemalloc_t* ma, u32 hint, maplf lf) {
  u32 cap = (hint == 0) ? 8 : perfectcap(hint, lf);
  rmem_t entries_mem = rmem_alloc_arrayt(ma, cap, smapent);
  if UNLIKELY(!entries_mem.p)
    return NULL;
  rmem_zerofill(entries_mem);
  u32 hash0 = fastrand();
  return smap_init(m, ma, entries_mem, cap, hash0, lf);
}

void smap_dispose(smap* m) {
  rmem_free(m->memalloc, m->entries_mem);
  #ifdef DEBUG
  rmem_zerofill(RMEM(m, sizeof(*m)));
  #endif
}

void smap_clear(smap* m) {
  m->len = 0;
  rmem_zerofill(RMEM(m->entries, m->cap*sizeof(smapent)));
}

static void smap_relocate(hash_t hash0, smapent* entries, u32 cap, smapent* ent) {
  usize index = hash_mem(ent->key, ent->keylen, hash0) & (cap - 1);
  while (entries[index].key) {
    if (streq(&entries[index], ent->key, ent->keylen))
      break;
    if (++index == cap)
      index = 0;
  }
  entries[index] = *ent;
}

static bool smap_grow(smap* m) {
  u32 newcap;
  if (check_mul_overflow(m->cap, (u32)2u, &newcap))
    return false;
  // dlog("grow len cap %u %u => cap size %u %zu",
  //   m->len, m->cap, newcap, (usize)newcap*sizeof(smapent));
  rmem_t entries_mem = rmem_alloc_arrayt(m->memalloc, newcap, smapent);
  if (!entries_mem.p)
    return false;
  // rehash
  smapent* new_entries = entries_mem.p;
  for (u32 i = 0; i < m->cap; i++) {
    smapent ent = m->entries[i];
    if (ent.key && ent.key != DELMARK)
      smap_relocate(m->hash0, new_entries, newcap, &ent);
  }
  rmem_free(m->memalloc, m->entries_mem);
  m->entries_mem = entries_mem;
  m->cap = newcap;
  m->gcap = newcap - (newcap >> m->lf);
  return true;
}

uintptr* nullable smap_assign(smap* m, const char* key, usize keylen) {
  if UNLIKELY(m->len >= m->gcap) {
    if (!smap_grow(m))
      return NULL;
  }
  usize index = hash_mem(key, keylen, m->hash0) & (m->cap - 1);
  while (m->entries[index].key) {
    if (streq(&m->entries[index], key, keylen))
      return &m->entries[index].value;
    if (m->entries[index].key == DELMARK) // recycle deleted slot
      break;
    if (++index == m->cap)
      index = 0;
  }
  m->len++;
  m->entries[index].key = key;
  m->entries[index].keylen = keylen;
  return &m->entries[index].value;
}

uintptr* nullable smap_lookup(const smap* m, const char* key, usize keylen) {
  usize index = hash_mem(key, keylen, m->hash0) & (m->cap - 1);
  while (m->entries[index].key) {
    if (streq(&m->entries[index], key, keylen))
      return &m->entries[index].value;
    if (++index == m->cap)
      index = 0;
  }
  return NULL;
}

bool smap_del(smap* m, const char* key, usize keylen) {
  void* vp = smap_lookup(m, key, keylen);
  if UNLIKELY(vp == NULL)
    return NULL;
  if (m->len == 1) {
    smap_clear(m); // clear all DELMARK entries
    return true;
  }
  smapent* ent = vp - offsetof(smapent,value);
  m->len--;
  ent->key = DELMARK;
  ent->keylen = 0;
  return true;
}

bool smap_itnext(const smap* m, const smapent** ep) {
  for (const smapent* e = (*ep)+1, *end = m->entries + m->cap; e != end; e++) {
    if (e->key && e->key != DELMARK) {
      *ep = e;
      return true;
    }
  }
  return false;
}

// the rest of this source file contains extra functions

// compaction
typedef struct cstate cstate;
struct cstate {
  smap*    m;
  smapent* entries;
};

static int cs_sort(const smapent* e1, const smapent* e2, cstate* cs) {
  smap* m = cs->m;
  bool null1 = e1->key == NULL || e1->key == DELMARK;
  bool null2 = e2->key == NULL || e2->key == DELMARK;
  if (null1 && null2) return 0;
  if (null1) return 1;
  if (null2) return -1;
  u32 i1 = hash_mem(e1->key, e1->keylen, cs->m->hash0) & (m->cap - 1);
  u32 i2 = hash_mem(e2->key, e2->keylen, cs->m->hash0) & (m->cap - 1);
  return i1 - i2;
}

static void insert_coll(smapent* entries, u32 cap, u32 index, const smapent* e) {
  #if DEBUG
  u32 n = 0;
  #endif
  while (entries[index].key) {
    assertf(!streq(&entries[index], e->key, e->keylen), "%.*s", (int)e->keylen, e->key);
    if (entries[index].key == DELMARK) // recycle deleted slot
      break;
    if (++index == cap)
      index = 0;
    assertf(++n < cap, "no free slots");
  }
  entries[index] = *e;
}

// smap_compact reorders the entries of m to minimize lookup distance.
// Has no effect if smap_score(m)==0.
// Returns false if scratch memory allocation in mem failed.
static bool smap_compact(smap* m, rmemalloc_t* ma) {
  rmem_t entries_mem = rmem_alloc_arrayt(ma, m->cap, smapent);
  if UNLIKELY(!entries_mem.p)
    return false;
  void* newentries = entries_mem.p;
  cstate cs = { m, newentries };

  // // copy of entries for later debug printing
  // smapent* entries_orig =
  //   rmem_alloc_aligned(ma, sizeof(smapent)*m->cap, _Alignof(smapent)).p;
  // memcpy(entries_orig, m->entries, sizeof(smapent)*m->cap);
  // smap m_orig = *m;
  // m_orig.entries = entries_orig;

  // copy entries that are not collisions
  for (u32 i = 0; i < m->cap; i++) {
    smapent* e = &m->entries[i];
    if (e->key == NULL || e->key == DELMARK) continue;
    u32 index = hash_mem(e->key, e->keylen, m->hash0) & (m->cap - 1);
    if (&m->entries[index] == e) { // not a collision; ideal slot
      cs.entries[index] = *e;
      e->key = NULL;
    }
  }

  // sort collision entries on index
  rsm_qsort(m->entries, m->cap, sizeof(smapent),
    (int(*)(const void*,const void*,void*))&cs_sort, &cs);
  // now, collisions only contains collision entries and looks like this:
  //   0 somekey collision on index 1
  //   1 somekey collision on index 3
  //   2 somekey collision on index 6
  //   3 somekey collision on index 20
  //   4 somekey collision on index 20
  //   5 NULL
  //   ... rest of array have NULL keys

  // insert ordered collision entries, starting in the middle
  usize count = 0;
  for (smapent* e = m->entries; e->key != NULL; e++)
    count++;
  for (usize i = 0; i < count; i++) {
    smapent* e = &m->entries[(i + count/2) % count];
    u32 index = hash_mem(e->key, e->keylen, m->hash0) & (m->cap - 1);
    insert_coll(cs.entries, m->cap, index, e);
  }

  memcpy(m->entries, newentries, sizeof(smapent)*m->cap);
  rmem_free(ma, entries_mem);

  return true;
}

// smap_score returns the sum of every collision scan distance,
// or 0 if m has perfect distribution.
static double smap_score(const smap* m) {
  double score = 0;
  for (const smapent* e = smap_itstart(m); smap_itnext(m, &e); ) {
    // cost             = scan_distance + nomatch_overscan
    // scan_distance    = abs(ideal_position - actual_position)
    // nomatch_overscan = stop_position - ideal_position
    // ideal_position   # where key hashes to
    // actual_position  # where entry is actually stored
    // stop_position    # closest NULL or DELMARK key after ideal_position

    // scan_distance
    usize ideal_i = hash_mem(e->key, e->keylen, m->hash0) & (m->cap - 1);
    usize actual_i = (usize)(e - m->entries); // actual position
    usize scan_dist = ideal_i > actual_i ? ideal_i - actual_i : actual_i - ideal_i;
    score += (double)scan_dist;

    // nomatch_overscan -- If a non-matching key hashes to this slot,
    // what is the cost of realizing it's not a match?
    for (u32 i = ideal_i; i < m->cap + ideal_i; i++) {
      const smapent* e = &m->entries[i % m->cap];
      if (e->key == NULL || e->key == DELMARK)
        break;
      score += 0.5;
    }
  }
  return score;
}

#ifdef DEBUG
UNUSED static void dump_smap(
  const smap* m, const smap* nullable beforem,
  const smapent** nullable highlightv, usize highlightc,
  int cmp);
#endif

double smap_optimize(smap* m, usize iterations, rmemalloc_t* ma) {
  // allocate temporary storage for a copy of m->entries
  rmem_t entries_mem = rmem_alloc_arrayt(ma, m->cap, smapent);
  if (!entries_mem.p)
    return -1.0;
  smapent* entries = entries_mem.p;
  memcpy(entries, m->entries, m->cap*sizeof(smapent));
  hash_t best_hash0 = m->hash0;
  double best_score = -1.0;

  #if defined(DEBUG) && !defined(__wasm__)
  smap m_orig = *m;
  m_orig.entries = entries;
  double score_orig = smap_score(m);
  usize iterations_total = iterations;
  #endif

  for (;;) {
    smap_clear(m);
    for (u32 i = 0; i < m->cap; i++) {
      smapent* e = &entries[i];
      if (e->key == NULL || e->key == DELMARK) continue;
      *smap_assign(m, e->key, e->keylen) = e->value;
    }
    if (iterations == 0) // done trying
      break;
    double score = smap_score(m);
    if (score < best_score || best_score < 0.0) {
      best_score = score;
      best_hash0 = m->hash0;
      if (score == 0.0) // perfect score
        break;
    }
    iterations--;
    m->hash0 = iterations ? fastrand() : best_hash0; // try a new seed or finalize w/ best

    #if defined(DEBUG) && !defined(__wasm__)
    if (iterations % 1000 == 0) {
      int t = (int)( (1.0f - ((float)iterations / iterations_total)) * 100.0f );
      fprintf(stderr, "\r\e[0K" "smap_optimize %.*s%.*s %u%%",
        t/2,      "||||||||||||||||||||||||||||||||||||||||||||||||||",
        50 - t/2, "..................................................", t);
    }
    #endif
  }

  // compact
  if (best_score != 0) {
    smap_compact(m, ma);
    double score = smap_score(m);
    if (score > best_score) { // undo
      for (u32 i = 0; i < m->cap; i++) {
        smapent* e = &entries[i];
        if (e->key == NULL || e->key == DELMARK) continue;
        *smap_assign(m, e->key, e->keylen) = e->value;
      }
    }
  }

  #if defined(DEBUG) && !defined(__wasm__)
  log("smap_optimize result: (score orig=%f, best=%f)\n"
    "───┬───────────────────┬───────────────────\n"
    "idx│  before           │  after\n"
    "───┼───────────────────┼───────────────────", score_orig, best_score);
  dump_smap(m, &m_orig, NULL, 0, 0);
  #endif

  rmem_free(ma, entries_mem);
  return best_score;
}


usize smap_cfmt(char* buf, usize bufcap, const smap* m, const char* name) {
  abuf_t s = abuf_make(buf, bufcap);
  abuf_fmt(&s, "static const smapent %s_entries[%u] = {\n ", name, m->cap);
  char* lnstart = s.p;
  for (u32 i = 0; i < m->cap; i++) {
    const smapent* e = &m->entries[i];
    char* chunkstart = s.p;
    for (;;) {
      if (e->key == NULL || e->key == ((const char*)1)) {
        abuf_str(&s, "{0},");
      } else {
        abuf_fmt(&s, "{\"%s\",%zu,%zu},", e->key, e->keylen, e->value);
      }
      if ((uintptr)(s.p - lnstart) <= 80)
        break;
      s.p = chunkstart; // rewind
      abuf_str(&s, "\n ");
      lnstart = s.p;
    }
  }
  s.p--; s.len--; // undo last ","
  abuf_fmt(&s,
    "};\n"
    "static const struct{u32 cap,len,gcap;maplf lf;hash_t hash0;const smapent* ep;}\n"
    "%s_data={%u,%u,%u,%u,0x%llx,%s_entries};\n"
    "static const smap* %s = (const smap*)&%s_data;",
    name, m->cap, m->len, m->gcap, m->lf, (u64)m->hash0, name, name, name
  );
  return abuf_terminate(&s);
}


usize smap_copy(smap* dstm, const smap* srcm, rmem_t entries_mem, rmemalloc_t* ma) {
  uintptr init_p = (uintptr)entries_mem.p;
  if (!rmem_align(&entries_mem, _Alignof(smapent)))
    return 0;
  if (entries_mem.size >= srcm->entries_mem.size) {
    memcpy(entries_mem.p, srcm->entries_mem.p, srcm->entries_mem.size);
    smap_init(dstm, ma, entries_mem, srcm->cap, srcm->hash0, srcm->lf);
  }
  usize align_offs = (usize)((uintptr)entries_mem.p - init_p);
  return srcm->entries_mem.size + align_offs;
}


// end of implementation -- debugging & testing stuff follows...
// --------------------------------------------------------------------------------

// dump_smap for printing/visualizing the state of a map
#ifdef DEBUG
#ifdef RSM_NO_LIBC
#define dump_smapent(...) ((void)0)
#else

static void dump_smapent(
  const smap* m, const smapent* e, const smapent** nullable hlv, usize hlc,
  int cmp, bool include_hash)
{
  if (e->key == NULL) {
    printf("  \e[2mNULL\e[22m            ");
    return;
  }
  if (e->key == ((const char*)1)) {
    printf("  \e[2mDEL\e[22m             ");
    return;
  }

  u32 index = (u32)(e - m->entries);
  hash_t h = hash_mem(e->key, e->keylen, m->hash0);
  usize ideal_index = h & (m->cap - 1);

  usize highlight = 0xffffffff;
  for (usize j = 0; j < hlc; j++) {
    if (assertnotnull(hlv[j])->key == e->key) {
      highlight = j;
      break;
    }
  }
  char color = '1'+(ideal_index%4);
  if (highlight != 0xffffffff) {
    printf("\e[48;5;%s""m", (highlight%2==0) ? "53" : "56");
  }

  printf("  %-5s", e->key);
  if (include_hash)
    printf(sizeof(hash_t) > 4 ? "  0x%016lx" : "  0x%08lx", h);

  if (ideal_index != index) {
    if (highlight != 0xffffffff) {
      printf("  (coll %2zu)", ideal_index);
    } else {
      printf("  (coll \e[9%cm%2zu\e[39m)", color, ideal_index);
    }
  } else {
    printf("           ");
  }
  if (highlight != 0xffffffff) {
    printf("\e[0;49m");
    if (cmp != 0)
      printf(" %s", highlight%2 ? cmp < 0 ? "▼" : "▲" : cmp < 0 ? "▲" : "▼");
  }
}
UNUSED static void dump_smap(
  const smap* m, const smap* nullable beforem,
  const smapent** nullable highlightv, usize highlightc,
  int cmp)
{
  for (u32 i = 0; i < m->cap; i++) {
    printf("\e[9%cm%2u\e[39m │", '1'+(i%4), i);
    if (beforem) {
      dump_smapent(beforem, &beforem->entries[i], highlightv, highlightc, 0, false);
      printf(" │");
    }
    dump_smapent(m, &m->entries[i], highlightv, highlightc, cmp, !beforem);
    printf("\n");
  }
}
#endif // RSM_NO_LIBC


// test & development function
#if 0

#include <stdlib.h>
#include <stdio.h>

__attribute__((used,constructor))
static void smap_test() {
  dlog("testing smap");

  u8 membuf[4096];
  rmem mem = rmem_mkbufalloc(membuf, sizeof(membuf));


  static const char* samples[] = {
    "i32", "div", "cmpgt", "and", "add", "brz", "brnz", "cmpeq", "cmplt", "i1",
    "fun", "i16", "move", "i8", "i64", "mod", "loadk", "shrs", "or", "mul", "shl",
    "ret", "sub", "shru", "xor",
  };
  usize nsamples = countof(samples);

  smap m = {0};
  // assertnotnull(smap_make(&m, mem, 0, MAPLF_2));
  assertnotnull(smap_make(&m, mem, nsamples, 5)); // perfect initial fit; no growth
  dlog("m.cap %u, m.gcap %u", m.cap, m.gcap);

  // // log hash values
  // for (usize i = 0; i < nsamples; i++) {
  //   hash_t h = hash_mem(samples[i], strlen(samples[i]), m.hash0);
  //   printf("%8llx %4llu %-5s\n", (u64)h, (u64)h % 8, samples[i]);
  // }

  dlog("insert all");

  for (usize i = 0; i < nsamples; i++) {
    uintptr* vp = smap_assign(&m, samples[i], strlen(samples[i]));
    assertf(vp != NULL, "vp=NULL  key=\"%s\"", samples[i]);
    *vp = i;
    assert(m.len == i+1);
  }
  for (usize i = 0; i < nsamples; i++) {
    uintptr* vp = smap_lookup(&m, samples[i], strlen(samples[i]));
    assertf(vp != NULL, "vp=NULL  key=\"%s\"", samples[i]);
    assertf(*vp == i,   "vp=%zu != i=%zu  key=\"%s\"", *vp, i, samples[i]);
  }

  dlog("memory usage: %zu B", (usize)(rmem_alloc(mem,1) - (void*)membuf) - RMEM_MK_MIN);

  dlog("smap_itstart & smap_itnext");

  for (smapent* e = smap_itstart(&m); smap_itnext(&m, &e); )
    printf("%s\t", e->key);
  printf("\n");

  dlog("insert same again");

  for (usize i = 0; i < nsamples; i++) {
    uintptr* vp = smap_assign(&m, samples[i], strlen(samples[i]));
    assertf(vp != NULL, "vp=NULL  key=\"%s\"", samples[i]);
    *vp = i;
  }
  for (usize i = 0; i < nsamples; i++) {
    uintptr* vp = smap_lookup(&m, samples[i], strlen(samples[i]));
    assertf(vp != NULL, "vp=NULL  key=\"%s\"", samples[i]);
    assertf(*vp == i,   "vp=%zu != i=%zu  key=\"%s\"", *vp, i, samples[i]);
  }

  dlog("delete all");

  for (usize i = 0; i < nsamples; i++) {
    bool ok = smap_del(&m, samples[i], strlen(samples[i]));
    assertf(ok, "smap_del \"%s\"", samples[i]);
  }
  for (usize i = 0; i < nsamples; i++) {
    uintptr* vp = smap_lookup(&m, samples[i], strlen(samples[i]));
    assertf(vp == NULL, "vp!=NULL  key=\"%s\"", samples[i]);
  }

  dlog("insert all");

  for (usize i = 0; i < nsamples; i++) {
    uintptr* vp = smap_assign(&m, samples[i], strlen(samples[i]));
    assertf(vp != NULL, "vp=NULL  key=\"%s\"", samples[i]);
    *vp = i;
  }
  for (usize i = 0; i < nsamples; i++) {
    uintptr* vp = smap_lookup(&m, samples[i], strlen(samples[i]));
    assertf(vp != NULL, "vp=NULL  key=\"%s\"", samples[i]);
    assertf(*vp == i,   "vp=%zu != i=%zu  key=\"%s\"", *vp, i, samples[i]);
  }

  smap_dispose(&m);
  exit(0);
}
#endif

#endif // defined(DEBUG)
