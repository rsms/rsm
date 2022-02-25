// hash map
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"

#define DELMARK ((const char*)1) /* assume no key is ever at address 0x1 */

// string hash function (FNV1a)
#if 1 // for when smap.cap uses u32
  #define FNV_BASE  0x811c9dc5
  #define FNV_PRIME 0x01000193 // pow(2,24) + pow(2,8) + 0x93
#else // for when smap.cap uses u64
  #define FNV_BASE  0xcbf29ce484222325ul
  #define FNV_PRIME 0x100000001b3lu // pow(2,40) + pow(2,8) + 0xb3
#endif

static u32 strhash(const void* data, usize len) {
  u32 hash = FNV_BASE;
  for (const u8* p = data, *end = data + len; p != end; ++p)
    hash = (*p ^ hash) * FNV_PRIME;
  return hash;
}

inline static bool smapenteq(const smapent* ent, const char* key, usize keylen) {
  return ent->keylen == keylen && memcmp(ent->key, key, keylen) == 0;
}

static const double captab[] = {
  /* MAPLF_1 */ (1.0 / 0.5),
  /* MAPLF_2 */ (1.0 / 0.75),
  /* MAPLF_3 */ (1.0 / 0.875),
  /* MAPLF_4 */ (1.0 / 0.9375),
};

static u32 perfectcap(u32 hint, maplf lf) {
  // TODO: figure out a way to compute optimal cap that is just shy of the
  // gcap growth threshold of hint using integer math instead of fp.
  return ALIGN2( (u32)((double)(ALIGN2(hint, 2)+1) * captab[lf-1]), 2);
}

usize smap_perfectsize(u32 hint, maplf lf) {
  usize nbytes;
  if (check_mul_overflow((usize)perfectcap(hint, lf), sizeof(smapent), &nbytes))
    return 0;
  return nbytes;
}

smap* nullable smap_make(smap* m, rmem mem, u32 hint, maplf lf) {
  m->mem = mem;
  m->entries = NULL;
  m->len = 0;
  m->cap = hint == 0 ? 8 : perfectcap(hint, lf);
  // lf is a bit shift magnitude that does fast integer division
  // i.e. cap-(cap>>lf) == (u32)((double)cap*0.75)
  m->lf = lf;
  m->gcap = m->cap - (m->cap >> m->lf);

  usize nbytes = smap_perfectsize(hint, lf);
  if (check_mul_overflow((usize)m->cap, sizeof(smapent), &nbytes))
    return NULL;
  m->entries = rmem_alloc(mem, nbytes);
  if UNLIKELY(m->entries == NULL)
    return NULL;
  memset(m->entries, 0, nbytes);
  return m;
}

void smap_dispose(smap* m) {
  rmem_free(m->mem, m->entries, m->cap*sizeof(smapent));
  #ifdef DEBUG
  memset(m, 0, sizeof(*m));
  #endif
}

void smap_clear(smap* m) {
  m->len = 0;
  memset(m->entries, 0, m->cap*sizeof(smapent));
}

static void smap_relocate(smapent* entries, u32 cap, smapent* ent) {
  u32 hash = strhash(ent->key, ent->keylen);
  u32 index = hash & (cap - 1);
  while (entries[index].key) {
    if (smapenteq(&entries[index], ent->key, ent->keylen))
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
  usize nbytes;
  if (check_mul_overflow((usize)newcap, sizeof(smapent), &nbytes))
    return false;
  // dlog("grow len cap %u %u => cap size %u %zu", m->len, m->cap, newcap, nbytes);
  smapent* new_entries = rmem_alloc(m->mem, nbytes);
  if (new_entries == NULL)
    return false;
  // rehash
  for (u32 i = 0; i < m->cap; i++) {
    smapent ent = m->entries[i];
    if (ent.key)
      smap_relocate(new_entries, newcap, &ent);
  }
  rmem_free(m->mem, m->entries, m->cap*sizeof(smapent));
  m->entries = new_entries;
  m->cap = newcap;
  m->gcap = newcap - (newcap >> m->lf);
  return true;
}

uintptr* nullable smap_assign(smap* m, const char* key, usize keylen) {
  if UNLIKELY(m->len >= m->gcap) {
    if (!smap_grow(m))
      return NULL;
  }
  u32 hash = strhash(key, keylen);
  u32 index = hash & (m->cap - 1);
  while (m->entries[index].key) {
    if (smapenteq(&m->entries[index], key, keylen))
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

uintptr* nullable smap_lookup(smap* m, const char* key, usize keylen) {
  u32 hash = strhash(key, keylen);
  u32 index = hash & (m->cap - 1);
  while (m->entries[index].key) {
    if (smapenteq(&m->entries[index], key, keylen))
      return &m->entries[index].value;
    if (++index == m->cap)
      index = 0;
  }
  return NULL;
}

uintptr* nullable smap_del(smap* m, const char* key, usize keylen) {
  void* vp = smap_lookup(m, key, keylen);
  if UNLIKELY(vp == NULL)
    return NULL;
  smapent* ent = vp - offsetof(smapent,value);
  m->len--;
  ent->key = DELMARK;
  ent->keylen = 0;
  return &ent->value;
}

bool smap_itnext(smap* m, smapent** ep) {
  smapent* e = (*ep)+1;
  smapent* end = m->entries + m->cap;
  while (e != end) {
    if (e->key && e->key != DELMARK) {
      *ep = e;
      return true;
    }
    e++;
  }
  return false;
}


// --------------------------------------
// test function
#if 0 && defined(DEBUG)

#include <stdlib.h>
#include <stdio.h>

__attribute__((used,constructor))
static void smap_test() {
  dlog("testing smap");

  u32 minhint = 2, maxhint = 16;

  for (maplf lf = 1; lf < 5; lf++) {
    printf("\nlf=%u      [gcap cap]", lf);
    printf("\n┌───");
    for (u32 hint = minhint; hint <= maxhint; hint += 2)
      printf(hint == minhint ? "─────────" : "┬─────────");
    printf("┐");

    printf("\n│ len");
    for (u32 hint = minhint; hint <= maxhint; hint += 2)
      printf(" %6u │ ", hint); // printf(" %4u │ ", ALIGN2(hint, 2));

    printf("\n├───");
    for (u32 hint = minhint; hint <= maxhint; hint += 2)
      printf(hint == minhint ? "─────────" : "┼─────────");
    printf("┤");

    printf("\n│    ");
    for (u32 hint = minhint; hint <= maxhint; hint += 2) {
      // 6 >= 8
      // 7 >= 8
      // 8 >= 8
      // map grows when inserting and len >= gcap
      // goal: find a formula for cap that results in: hint == gcap+1
      // i.e. we want: gcap to be +1 of hint
      //
      // cap to be sufficiently large for hint
      //
      u32 cap = ALIGN2(hint, 2);

      //   1 = 0.5
      //   2 = 0.75
      //   3 = 0.877
      //   4 = 0.93777

      u32 cap1 = cap;
      if (lf == 1) cap = ALIGN2( (u32)((double)(cap+1) * (1.0 / 0.5)) , 2);
      if (lf == 2) cap = ALIGN2( (u32)((double)(cap+1) * (1.0 / 0.75)) , 2);
      if (lf == 3) cap = ALIGN2( (u32)((double)(cap+1) * (1.0 / 0.875)) , 2);
      if (lf == 4) cap = ALIGN2( (u32)((double)(cap+1) * (1.0 / 0.9375)) , 2);
      // 1-1/2
      // 1-1/2/2
      // 1-1/2/2/2
      // 1-1/2/2/2/2

      // u32 cap2 = ALIGN2(cap1 + 1 + ((cap1 / 1) >> (lf - 1)) ,2);
      // u32 cap2 = ALIGN2(hint + 1 + ((hint / 1) >> (lf - 1)), 2);

      static const double tab[] = {
        (1.0 / 0.5),
        (1.0 / 0.75),
        (1.0 / 0.875),
        (1.0 / 0.9375),
      };

      u32 cap2 = ALIGN2( (u32)((double)(cap1+1) * tab[lf-1]) , 2);

      u32 gcap = cap2 - (cap2 >> lf);

      char color =
        gcap <= hint ? '1' : // cap too small for hint
        cap2 > cap   ? '3' : // cap2 calc not ideal, but works
                       '2' ; // cap2 calc is perfect
      printf("\e[9%cm\e[2m%3u\e[22m %3d\e[39m │ ", color, cap2, cap);
    }

    printf("\n└───");
    for (u32 hint = minhint; hint <= maxhint; hint += 2)
      printf(hint == minhint ? "─────────" : "┴─────────");
    printf("┘\n");
  }
  printf("\n");

  u8 membuf[4096];
  rmem mem = rmem_mkbufalloc(membuf, sizeof(membuf));

  smap m = {0};
  assertnotnull(smap_make(&m, mem, 0, MAPLF_2));

  static const char* samples[] = {
    "i32", "div", "cmpgt", "and", "add", "brz", "brnz", "cmpeq", "cmplt", "i1",
    "fun", "i16", "move", "i8", "i64", "mod", "loadk", "shrs", "or", "mul", "shl",
    "ret", "sub", "shru", "xor",
  };
  usize nsamples = countof(samples);

  // // log hash values
  // for (usize i = 0; i < nsamples; i++) {
  //   u32 h = strhash(samples[i], strlen(samples[i]));
  //   printf("%8x %4u %-5s\n", h, h % 8, samples[i]);
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
    uintptr* vp = smap_del(&m, samples[i], strlen(samples[i]));
    assertf(vp != NULL, "vp=NULL  key=\"%s\"", samples[i]);
    assertf(*vp == i,   "vp=%zu != i=%zu  key=\"%s\"", *vp, i, samples[i]);
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
