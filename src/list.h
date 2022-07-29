// intrusive doubly-linked list
// SPDX-License-Identifier: Apache-2.0

RSM_ASSUME_NONNULL_BEGIN

typedef struct ilist {
  struct ilist* next;
  struct ilist* prev;
} ilist_t;

#define ILIST_INIT_HEAD(NAME) { &(NAME), &(NAME) }
#define ILIST_HEAD(NAME)      ilist_t NAME = ILIST_INIT_HEAD(NAME)

// ilist_init sets list to point to itself
inline static void ilist_init(ilist_t* list) {
  list->next = list;
  list->prev = list;
}


// ilist_is_empty returns true if a list is empty
inline static bool ilist_is_empty(const ilist_t* head) {
  return head->next == head;
}

// list_is_first returns true if entry is the first entry in list head
inline static bool ilist_is_first(const ilist_t* head, const ilist_t* entry) {
  return entry->prev == head;
}

// list_is_last returns true if entry is the last entry in list head
inline static bool ilist_is_last(const ilist_t* head, const ilist_t* entry) {
  return entry->next == head;
}

// ilist_is_head returns true if entry is the list head
inline static bool ilist_is_head(const ilist_t* head, const ilist_t* entry) {
  return entry == head;
}


// _ilist_insert inserts newent in between prev and next
inline static void _ilist_insert(ilist_t* newent, ilist_t* prev, ilist_t* next) {
  //                     ┌────────────┐
  //            ┏━[prev]━┥   newent   ┝━[next]━┓
  //            ┃        └────────────┘        ┃
  //            ▼             ▲   ▲            ▼
  //    ┌────────────┐        ┃   ┃        ┌────────────┐
  //... │    prev    ┝━[next]━┛   ┗━[prev]━┥    next    │ ...
  //    └────────────┘                     └────────────┘
  //
  next->prev = newent;
  newent->next = next;
  newent->prev = prev;
  prev->next = newent;
}

// ilist_insert_after adds a new entry after refent
inline static void ilist_insert_after(ilist_t* refent, ilist_t* newent) {
  _ilist_insert(newent, refent, refent->next);
}

// ilist_insert_before adds a new entry before refent
inline static void ilist_insert_before(ilist_t* refent, ilist_t* newent) {
  _ilist_insert(newent, refent->prev, refent);
}

// ilist_prepend adds a new entry to the beginning of a list
inline static void ilist_prepend(ilist_t* head, ilist_t* newent) {
  ilist_insert_after(head, newent);
}

// ilist_append adds a new entry to the end of a list
inline static void ilist_append(ilist_t* head, ilist_t* newent) {
  ilist_insert_before(head, newent);
}


// ilist_del removes entry from its list
inline static void ilist_del(ilist_t* entry) {
  ilist_t* prev = entry->prev;
  ilist_t* next = entry->next;
  next->prev = prev;
  prev->next = next;

  entry->next = GENERIC_POISON1;
  entry->prev = GENERIC_POISON2;
}

// list_del_init deletes entry from list and reinitialize it as an empty list
inline static void ilist_del_init(ilist_t* entry) {
  ilist_del(entry);
  ilist_init(entry);
}


// ilist_pop removes & returns the last entry from list "head".
// Returns NULL if the list is empty.
inline static ilist_t* nullable ilist_pop(ilist_t* head) {
  ilist_t* last = head->prev;
  if (head == last) // empty list
    return NULL;
  ilist_del(last);
  return last;
}


// ilist_replace replaces one entry with another.
// oldent is no longer in the list after this.
inline static void ilist_replace(ilist_t* oldent, ilist_t* newent) {
  newent->next = oldent->next;
  newent->next->prev = newent;
  newent->prev = oldent->prev;
  newent->prev->next = newent;
}


// ilist_swap swaps the positions of entry1 and entry2
inline static void ilist_swap(ilist_t* entry1, ilist_t* entry2) {
  ilist_t* cursor = entry2->prev;
  ilist_del(entry2);
  ilist_replace(entry1, entry2);
  if (cursor == entry1)
    cursor = entry2;
  ilist_insert_after(cursor, entry1);
}


// ilist_move removes an entry src_entry from its list and add it after dst_head
inline static void ilist_move(ilist_t* src_entry, ilist_t* dst_head) {
  ilist_del(src_entry);
  ilist_insert_after(dst_head, src_entry);
}


// ilist_entry returns the embedding struct for (container of) this list entry
#define ilist_entry(ilist_ptr, container_type, container_member) \
  container_of(ilist_ptr, container_type, container_member)

// ilist_first_entry returns the first entry of a list
#define ilist_first_entry(head_ptr, entstruct_type, entstruct_member) \
  ilist_entry(assertnotnull((head_ptr)->next), entstruct_type, entstruct_member)

// ilist_last_entry returns the last entry of a list
#define ilist_last_entry(head_ptr, entstruct_type, entstruct_member) \
  ilist_entry(assertnotnull((head_ptr)->prev), entstruct_type, entstruct_member)

// ilist_maybe_first_entry returns the first entry of a list,
// or NULL if the list is empty.
#define ilist_maybe_first_entry(head_ptr, entstruct_type, entstruct_member) ({ \
  ilist_t* head__ = (head_ptr); \
  ilist_t* cur__ = head__->next; \
  cur__ != head__ ? ilist_entry(cur__, entstruct_type, entstruct_member) : NULL; \
})

// ilist_next_entry returns the next entry of a list.
// On end of list, returns a pointer to the parent list container.
#define ilist_next_entry(cursor_ptr, entstruct_member) \
  ilist_entry((cursor_ptr)->entstruct_member.next, \
              __typeof__(*(cursor_ptr)), entstruct_member)

// ilist_next_entry_wrap returns the next entry of a list
// On end of list, wraps around (or "rewinds") and returns the first list entry.
#define ilist_next_entry_wrap(cursor_ptr, parent_head_ptr, entstruct_member) \
  (ilist_is_last(parent_head_ptr, &(cursor_ptr)->entstruct_member) ? \
    ilist_first_entry(parent_head_ptr, __typeof__(*(cursor_ptr)), entstruct_member) : \
    ilist_next_entry(cursor_ptr, entstruct_member))


// ilist_for_each iterates over a list
#define ilist_for_each(CUR, head) \
  for (ilist_t* CUR = (head)->next; !ilist_is_head((head), CUR); CUR = CUR->next)

// ilist_for_each_reverse iterates over a list backwards
#define ilist_for_each_reverse(CUR, head) \
  for (ilist_t* CUR = (head)->prev; !ilist_is_head((head), CUR); CUR = CUR->prev)

// ilist_for_each_safe iterates over a list, safe against removal of list entries
#define ilist_for_each_safe(CUR, head) \
  for (ilist_t* CUR = (head)->next, *CUR ## _next_tmp__ = CUR->next; \
       !ilist_is_head((head), CUR); \
       CUR = CUR ## _next_tmp__, CUR ## _next_tmp__ = CUR->next)

// ilist_for_each_reverse_safe iterates over a list backwards,
// safe against removal of list entries
#define ilist_for_each_reverse_safe(CUR, head) \
  for (ilist_t* CUR = (head)->prev, *CUR ## _next_tmp__ = CUR->prev; \
       !ilist_is_head((head), CUR); \
       CUR = CUR ## _next_tmp__, CUR ## _next_tmp__ = CUR->prev)

// ilist_for_each_continue continues iteration over a list.
// Useful for nested for-each loops.
#define ilist_for_each_continue(CUR, head) \
  for (CUR = CUR->next; !list_is_head(CUR, (head)); CUR = CUR->next)

// ilist_for_eachx iterates over a list using an existing ilist_t* local
#define ilist_for_eachx(CUR, head) \
  for (CUR = (head)->next; !ilist_is_head((head), CUR); CUR = CUR->next)

// ilist_for_each_reverse iterates over a list backwards using an existing ilist_t* local
#define ilist_for_each_reversex(CUR, head) \
  for (CUR = (head)->prev; !ilist_is_head((head), CUR); CUR = CUR->prev)


//——————————————————————————————————————————————————————————————————————————————————————
#ifdef ILIST_TEST_IMPL

typedef struct {
  int x;
  ilist_t list;
} ilist_test_t;

typedef struct ilist_test_ent {
  int a;
  ilist_t listx; // different name to find bugs
} ilist_test_ent_t;


UNUSED static void ilist_test_dump(const ilist_t* head) {
  u32 i = 0;
  ilist_for_each(ent, head) {
    dlog("  [%u] => ents[%d]", i++, ilist_entry(ent, ilist_test_ent_t, listx)->a);
  }
}

static void ilist_test() {
  ilist_test_ent_t ents[] = { {.a=0}, {.a=1}, {.a=2}, {.a=3} };

  ilist_test_t list_ = {0};
  ilist_test_t* t = &list_;

  // initialize list; list is now ()
  // note: we do not need to call ilist_init on entries since they
  // are not used as lists.
  ilist_init(&t->list);
  assert(ilist_is_empty(&t->list));
  assert(ilist_is_head(&t->list, t->list.next));
  assert(ilist_is_head(&t->list, t->list.prev));
  assertnull(ilist_maybe_first_entry(&t->list, ilist_test_ent_t, listx));

  // append ents[0]; list is now (0)
  ilist_insert_after(&t->list, &ents[0].listx);

  // ilist_entry accesses the container of a list or list entry
  ilist_test_t* tp = ilist_entry(&t->list, ilist_test_t, list);
  assert(tp == t);
  ilist_test_ent_t* ent = ilist_entry(&ents[0].listx, ilist_test_ent_t, listx);
  assert(ent == &ents[0]);

  // access first entry
  ent = ilist_first_entry(&t->list, ilist_test_ent_t, listx);
  assert(ent == &ents[0]);
  assert(ilist_is_first(&t->list, &ent->listx));

  // append ents[3] & [2]; list is now (0 1 2)
  ilist_append(&t->list, &ents[1].listx);
  ilist_append(&t->list, &ents[2].listx);
  ent = ilist_next_entry(ent, listx); assert(ent == &ents[1]);
  ent = ilist_next_entry(ent, listx); assert(ent == &ents[2]);
  assert(ilist_is_last(&t->list, &ent->listx));

  // end of list is a pointer to the list container itself
  ent = ilist_next_entry(ent, listx);
  assert(ent == (void*)t);

  // verify ilist_for_each works as expected
  int i = 0; // expect ents[0] first
  ilist_for_each(cur, &t->list) {
    ilist_test_ent_t* ent = ilist_entry(cur, ilist_test_ent_t, listx);
    assertf(ent == &ents[i], "ent->a=%d, expect %d", ent->a, i);
    i++;
  }

  // verify ilist_for_each_reverse works as expected
  i = 2; // expect ents[2] first
  ilist_for_each_reverse(cur, &t->list) {
    ilist_test_ent_t* ent = ilist_entry(cur, ilist_test_ent_t, listx);
    assertf(ent == &ents[i], "ent->a=%d, expect %d", ent->a, i);
    i--;
  }

  // ilist_next_entry_wrap wraps around instead of ending
  ent = ilist_first_entry(&t->list, ilist_test_ent_t, listx); assert(ent == &ents[0]);
  ent = ilist_next_entry_wrap(ent, &t->list, listx); assert(ent == &ents[1]);
  ent = ilist_next_entry_wrap(ent, &t->list, listx); assert(ent == &ents[2]);
  ent = ilist_next_entry_wrap(ent, &t->list, listx); assert(ent == &ents[0]); // wraps

  // insert ents[3] before ents[2]; list is now (0 1 3 2)
  ilist_insert_before(&ents[2].listx, &ents[3].listx);
  ent = ilist_first_entry(&t->list, ilist_test_ent_t, listx); assert(ent == &ents[0]);
  ent = ilist_next_entry(ent, listx); assert(ent == &ents[1]);
  ent = ilist_next_entry(ent, listx); assert(ent == &ents[3]);
  ent = ilist_next_entry(ent, listx); assert(ent == &ents[2]);
  assert(ilist_is_last(&t->list, &ent->listx));

  // remove ents[3]; list is now (0 1 2)
  ilist_del(&ents[3].listx);
  ent = ilist_first_entry(&t->list, ilist_test_ent_t, listx); assert(ent == &ents[0]);
  ent = ilist_next_entry(ent, listx); assert(ent == &ents[1]);
  ent = ilist_next_entry(ent, listx); assert(ent == &ents[2]);
  assert(ilist_is_last(&t->list, &ent->listx));

  // replace ents[1] with ents[3]; list is now (0 3 2)
  ilist_replace(&ents[1].listx, &ents[3].listx);
  ent = ilist_first_entry(&t->list, ilist_test_ent_t, listx); assert(ent == &ents[0]);
  ent = ilist_next_entry(ent, listx); assert(ent == &ents[3]);
  ent = ilist_next_entry(ent, listx); assert(ent == &ents[2]);
  assert(ilist_is_last(&t->list, &ent->listx));

  // prepend ents[1]; list is now (1 0 3 2)
  ilist_prepend(&t->list, &ents[1].listx);
  ent = ilist_first_entry(&t->list, ilist_test_ent_t, listx); assert(ent == &ents[1]);
  ent = ilist_next_entry(ent, listx); assert(ent == &ents[0]);
  ent = ilist_next_entry(ent, listx); assert(ent == &ents[3]);
  ent = ilist_next_entry(ent, listx); assert(ent == &ents[2]);
  assert(ilist_is_last(&t->list, &ent->listx));

  // swap ents[0] <> ents[2]; list is now (1 2 3 0)
  ilist_swap(&ents[0].listx, &ents[2].listx);
  ent = ilist_first_entry(&t->list, ilist_test_ent_t, listx); assert(ent == &ents[1]);
  ent = ilist_next_entry(ent, listx); assert(ent == &ents[2]);
  ent = ilist_next_entry(ent, listx); assert(ent == &ents[3]);
  ent = ilist_next_entry(ent, listx); assert(ent == &ents[0]);
  assert(ilist_is_last(&t->list, &ent->listx));

  // create a second list
  ILIST_HEAD(list2); // ilist_t list2 = { &(list2), &(list2) };
  assert(ilist_is_empty(&list2));
  assert(ilist_is_head(&list2, list2.next));
  assert(ilist_is_head(&list2, list2.prev));
  assertnull(ilist_maybe_first_entry(&list2, ilist_test_ent_t, listx));

  // move ents[3] to list2
  ilist_move(&ents[3].listx, &list2);
  // t.list=(1 2 0)
  ent = ilist_first_entry(&t->list, ilist_test_ent_t, listx); assert(ent == &ents[1]);
  ent = ilist_next_entry(ent, listx); assert(ent == &ents[2]);
  ent = ilist_next_entry(ent, listx); assert(ent == &ents[0]);
  assert(ilist_is_last(&t->list, &ent->listx));
  // list2=(3)
  ent = ilist_first_entry(&list2, ilist_test_ent_t, listx); assert(ent == &ents[3]);
  assert(ilist_is_last(&list2, &ent->listx));

  // move ents[0] to list2
  ilist_move(&ents[0].listx, &ents[3].listx);
  // t.list=(1 2)
  ent = ilist_first_entry(&t->list, ilist_test_ent_t, listx); assert(ent == &ents[1]);
  ent = ilist_next_entry(ent, listx); assert(ent == &ents[2]);
  assert(ilist_is_last(&t->list, &ent->listx));
  // list2=(3 0)
  ent = ilist_first_entry(&list2, ilist_test_ent_t, listx); assert(ent == &ents[3]);
  ent = ilist_next_entry(ent, listx); assert(ent == &ents[0]);
  assert(ilist_is_last(&list2, &ent->listx));

  // move ents[2] to list2, inserting it after ents[3]
  ilist_move(&ents[2].listx, &ents[3].listx);
  // dlog("t.list:"); ilist_test_dump(&t->list);
  // dlog("list2:"); ilist_test_dump(&list2);
  // t.list=(1)
  ent = ilist_first_entry(&t->list, ilist_test_ent_t, listx); assert(ent == &ents[1]);
  assert(ilist_is_last(&t->list, &ent->listx));
  // list2=(3 2 0)
  ent = ilist_first_entry(&list2, ilist_test_ent_t, listx); assert(ent == &ents[3]);
  ent = ilist_next_entry(ent, listx); assert(ent == &ents[2]);
  ent = ilist_next_entry(ent, listx); assert(ent == &ents[0]);
  assert(ilist_is_last(&list2, &ent->listx));

  // move ents[1] to list2 and swap entries to make list2 sorted; list2=(0 1 2 3)
  ilist_move(&ents[1].listx, &ents[3].listx);
  ilist_swap(&ents[3].listx, &ents[0].listx);

  // ilist_for_each_safe allows for removal of entries during iteration
  i = 0;
  ilist_for_each_safe(cur, &list2) {
    ilist_test_ent_t* ent = ilist_entry(cur, ilist_test_ent_t, listx);
    assertf(ent == &ents[i], "ent->a=%d, expect %d", ent->a, i);
    ilist_del(cur);
    i++;
  }

}

#endif

RSM_ASSUME_NONNULL_END
